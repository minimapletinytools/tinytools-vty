{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Manipulator (
  ManipulatorWidgetConfig(..)
  , ManipulatorWidget(..)
  , holdManipulatorWidget
) where

import           Relude


import           Potato.Flow
import           Potato.Reflex.Vty.Widget
import Potato.Flow.Reflex.Vty.CanvasPane
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Reflex.Vty.Helpers


import           Data.Constraint.Extras (Has')
import Control.Lens (over, _1)
import           Control.Monad.Fix
import           Data.Dependent.Sum        (DSum ((:=>)))
import qualified Data.IntMap.Strict as IM
import Data.These
import Data.Maybe (fromJust)

import Reflex.Potato.Helpers
import           Reflex
import           Reflex.Network
import           Reflex.Vty
import qualified Graphics.Vty as V


data ManipState = ManipStart | Manipulating | ManipEnd deriving (Show, Eq)

needUndoFirst :: ManipState -> Bool
needUndoFirst ManipStart = False
needUndoFirst _ = True

data HandleConfig t = HandleConfig {
  _handleConfig_pfctx :: PFWidgetCtx t
  , _handleConfig_position :: Behavior t (Int, Int)
  , _handleConfig_graphic :: Behavior t Char
  , _handleConfig_dragEv :: Event t ((Int,Int), Drag2)
}

-- TODO this needs to be able to render lines as well (or maybe that's a diff function)
makeHandle :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => HandleConfig t
  -> VtyWidget t m (Event t (ManipState, (Int, Int))) -- ^ (manipulation state, drag to position)
makeHandle HandleConfig {..} = do
  -- TODO drag events (must handle proper focus)
    -- if down on dpos begin tracking
    -- if up flip end flag

  -- draw image
  -- TODO use _pFWidgetCtx_attr_manipulator attribute
  --(x,y) <- sample bpos
  tellImages $ ffor
    (ffor3 _handleConfig_position _handleConfig_graphic (current . _pFWidgetCtx_attr_manipulator $ _handleConfig_pfctx) (,,))
    $ \((x,y),graphic,attr) -> [V.translate x y $ V.charFill attr graphic 1 1]
  return undefined


data ManipulatorWidgetConfig t = ManipulatorWidgetConfig {
  _manipulatorWigetConfig_pfctx :: PFWidgetCtx t
  , _manipulatorWigetConfig_selected  :: Dynamic t (Bool, Selected)
  , _manipulatorWidgetConfig_panPos :: Behavior t (Int, Int)
  , _manipulatorWidgetConfig_drag   :: Event t ((CursorState, (Int,Int)), Drag2)
  -- TODO
  --, _manipulatorWidgetConfig_cancel :: Event t ()
  -- TODO
  --, _manipulatorWidgetConfig_justCreated :: Behavior t Bool
}

data ManipulatorWidget t = ManipulatorWidget {
  _manipulatorWidget_modify :: Event t (Bool, ControllersWithId) -- ^ first param is whether we should undo previous action or not
  , _manipulatorWidget_manipulating :: Dynamic t Bool
}

data ManipSelectionType = MSTNone | MSTBox | MSTLine | MSTText | MSTBBox deriving (Show, Eq)

holdManipulatorWidget :: forall t m. (Reflex t, MonadHold t m, MonadFix m, NotReady t m, Adjustable t m, PostBuild t m, Has' Show MTag Identity)
  => ManipulatorWidgetConfig t
  -> VtyWidget t m (ManipulatorWidget t)
holdManipulatorWidget ManipulatorWidgetConfig {..} = mdo
  let
    dragStart = cursorDragStateEv Nothing (Just DragStart) _manipulatorWidgetConfig_drag
    dragging = cursorDragStateEv Nothing (Just Dragging) _manipulatorWidgetConfig_drag
    dragEnd = cursorDragStateEv Nothing (Just DragEnd) _manipulatorWidgetConfig_drag
    selectionChangedEv = updated _manipulatorWigetConfig_selected

  -- Tracks whether we're manipulating. This is needed so that we don't undo the first manipulation event.
  bManipulating <- return . current
    =<< (holdDyn False $ leftmost [dragEnd $> False, modifyEv $> True])

  dynManipulator <- toManipulator $ fmap snd selectionChangedEv
  -- see comments on 'manipWidget'
  dynManipSelTypeChange' <- holdDyn MSTNone $ ffor (updated dynManipulator) $ \case
    (MTagBox :=> _) -> MSTBox
    (MTagLine :=> _) -> MSTLine
    (MTagText :=> _) -> MSTText
    (MTagBoundingBox :=> _) -> MSTBBox
    _ -> MSTNone
  dynManipSelTypeChange <- holdUniqDyn dynManipSelTypeChange'

  -- NEW STUFF
  let
    selectManip :: MTag a -> Event t (Bool, a)
    selectManip mtag = r where
      selectManip' = select (fanDSum (updated dynManipulator)) mtag
      -- NOTE this completely negates the performance of using select/fan, you need to stick the tuple inside the DSum to do this right
      alignfn (These ns m) = Just (ns, m)
      alignfn _ = Nothing
      r = alignEventWithMaybe alignfn (fmap fst $ selectionChangedEv) selectManip'


  -- TODO you should prob split into functions...
  -- BOX MANIPULATOR
  let
    boxManip_selectedEv = selectManip MTagBox
    boxManip_dmBox = fmap snd boxManip_selectedEv
    boxManip_newElt = fmap fst boxManip_selectedEv
    boxManip_dlbox = fmap _mBox_box boxManip_dmBox
  boxManip_dynBox <- holdDyn Nothing (fmap Just boxManip_dmBox)
  let
    boxManip :: VtyWidget t m (Event t (ManipState, ControllersWithId))
    boxManip = do
      brDyn' <- holdDyn (LBox 0 0) boxManip_dlbox
      let brBeh = ffor2 _manipulatorWidgetConfig_panPos (current brDyn') (\(px, py) (LBox (V2 x y) (V2 w h)) -> (x+px+w, y+py+h))
      makeHandle $ HandleConfig {
          _handleConfig_pfctx = _manipulatorWigetConfig_pfctx
          , _handleConfig_position = brBeh
          , _handleConfig_graphic = constant '┌'
          , _handleConfig_dragEv = cursorDragStateEv (Just CSBox) Nothing _manipulatorWidgetConfig_drag
        }

      debugStream [
        never
        --, fmapLabelShow "modify" modifyEv
        ]

      -- TODO do this properly
      -- for now we assume brBeh is always the active handle
      let
        pushfn :: (Maybe MBox, ((Int,Int), Drag2)) -> PushM t (Maybe (ManipState, ControllersWithId))
        pushfn (mmbox, (_, Drag2 (fromX, fromY) (toX, toY) _ _ _)) = if isNothing mmbox then return Nothing else do
          wasManipulating <- sample bManipulating
          let
            -- TODO temp, do this properly
            ms = if not wasManipulating then ManipStart else Manipulating
            MBox {..} = fromJust mmbox
            r = CTagBox :=> (Identity $ CBox {
                _cBox_deltaBox = DeltaLBox 0 $ V2 (toX-fromX) (toY-fromY)
              })
          return . Just $ (ms, IM.singleton _mBox_target r) where
        pushinputev = attach (current boxManip_dynBox) $ (cursorDragStateEv (Just CSBox) (Just Dragging) _manipulatorWidgetConfig_drag)
      return $ push pushfn pushinputev

    finalManip :: Event t (VtyWidget t m (Event t (ManipState, ControllersWithId)))
    finalManip = ffor (updated dynManipSelTypeChange) $ \case
      MSTBox -> boxManip
      _ -> return never
    -- TODO the rest of them

  -- NOTE the 'networkHold' here doesn't seem to play well with other places where I use 'runWithAdjust'
  -- thus, we use 'dynManipSelTypeChange' above instead to limit the number of times the widget changes (even if nothing actually changes)
  -- CORRECTION, this is probbaly just because dynamics inside manip widgets are getting recreated by networkHold and less related to runWithAdjust conflicts
  -- still better to have fewer network updates like this.
  manipWidget :: Dynamic t (Event t (ManipState, ControllersWithId))
    <- networkHold (return never) finalManip
  let
    modifyEv :: Event t (Bool, ControllersWithId)
    modifyEv = fmap (over _1 needUndoFirst) $ switchDyn manipWidget


  debugStream [
    never
    --, fmapLabelShow "dynManip" $ selectManip MTagBox
    ]

  return
    ManipulatorWidget {
      -- TODO
      _manipulatorWidget_modify = modifyEv
      -- TODO
      , _manipulatorWidget_manipulating = undefined
    }
