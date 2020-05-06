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

-- TODO this needs to be able to render lines as well (or maybe that's a diff function)
makeHandle :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => Behavior t (Int, Int) -- ^ position of handle
  -> Char -- ^ handle graphic
  -> VtyWidget t m (Event t (ManipState, (Int, Int))) -- ^ (manipulation state, drag to position)
makeHandle bpos graphic = do
  -- TODO drag events (must handle proper focus)
    -- if down on dpos begin tracking
    -- if up flip end flag

  -- draw image
  -- TODO use _pFWidgetCtx_attr_manipulator attribute
  --(x,y) <- sample bpos
  tellImages $ ffor (bpos) $ \(x,y) -> [V.translate x y $ V.charFill V.defAttr graphic 1 1]
  return undefined


data ManipulatorWidgetConfig t = ManipulatorWidgetConfig {
  _manipulatorWigetConfig_selected  :: Dynamic t (Bool, Selected)
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


  let
    boxManip :: VtyWidget t m (Event t (ManipState, ControllersWithId))
    boxManip = do

      -- TODO this needs to be sampled inside a foldDynM to work
      (px, py) <- sample _manipulatorWidgetConfig_panPos

      let
        selectedEv = selectManip MTagBox
        dmBox = fmap snd selectedEv
        newElt = fmap fst selectedEv
        dlbox = fmap _mBox_box dmBox
      -- TODO due to late creation of boxManip, this misses out on the first box change event
      -- you can solve this easily by moving this stuff outside of boxManip
      dynBox <- holdDyn Nothing (fmap Just dmBox)

      brBeh <- hold (20,20) $ fmap (\(LBox (V2 x y) (V2 w h)) -> (x+px+w, y+py+h)) dlbox
      makeHandle brBeh '┌'

      debugStream [
        never
        --, fmapLabelShow "dlbox" selectedEv
        --, fmapLabelShow "dragging" _manipulatorWidgetConfig_drag
        --, fmapLabelShow "cursor" $ cursorDragStateEv (Just CSBox) (Just Dragging) _manipulatorWidgetConfig_drag
        --, fmapLabelShow "modify" modifyEv
        --, fmapLabelShow "dynManip" $ selectManip MTagBox
        ]

      -- TODO do this properly
      -- for now we assume brBeh is always the active handle
      let
        -- TODO get mmbox inside in a not terrible way...
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
        -- sucks that I have to use attachPromptly here :(, you could change this to regular attach but you would have to
        -- 1. properly handle the Nothing case in pushfn
        -- 2. reset dynBox to Nothing on switch, so that it doesn't render the previously selected box
        pushinputev = attach (current dynBox) $ (cursorDragStateEv (Just CSBox) (Just Dragging) _manipulatorWidgetConfig_drag)
      return $ push pushfn pushinputev

    finalManip :: Event t (VtyWidget t m (Event t (ManipState, ControllersWithId)))
    finalManip = ffor (updated dynManipSelTypeChange) $ \case
      MSTBox -> boxManip
      _ -> return never
    -- TODO the rest of them

  -- NOTE the 'networkHold' here doesn't seem to play well with other places where I use 'runWithAdjust'
  -- thus, we use 'dynManipSelTypeChange' above instead to limit the number of times the widget changes (even if nothing actually changes)
  manipWidget :: Dynamic t (Event t (ManipState, ControllersWithId))
    <- networkHold (return never) finalManip
  --  <- boxManip >>= return . constDyn
  let
    modifyEv :: Event t (Bool, ControllersWithId)
    modifyEv = fmap (over _1 needUndoFirst) $ switchDyn manipWidget


  debugStream [
    never
    --, updated manipWidget $> "manip updated"
    --, fmapLabelShow "poop" selectionChangedEv
    --, fmapLabelShow "cursor" $ cursorDragStateEv (Just CSBox) (Just Dragging) _manipulatorWidgetConfig_drag
    --, fmapLabelShow "modify" modifyEv
    --, fmapLabelShow "dynManip" $ selectManip MTagBox
    ]

{-
    --- OLD STUFF
    -- recreate the manipulator each time the selection changes
  let
    mapfn :: Selected -> VtyWidget t m (Event t (Bool, ControllersWithId))
    mapfn _ = mdo
      -- TODO hook up to didStart
      let
        -- TODO figure this out
        dragging = cursorDragStateEv (Just CSBox) (Just Dragging) _manipulatorWidgetConfig_drag
        dragEnd = cursorDragStateEv Nothing (Just DragEnd) _manipulatorWidgetConfig_drag
      wasManip <- holdDyn False $ leftmost [didStart $> True, dragging $> True, dragEnd $> False]

      manipulator <- sample $ current dynManipulator
      (didStart, w) <- case manipulator of
        (MTagBox :=> Identity (MBox {..})) -> do
          let
            LBox (V2 x y) (V2 w h) = _mBox_box
            -- TODO draw 4 corner images
            -- TODO create 8 drag events
            pushfn :: ((Int,Int), Drag2) -> PushM t (Maybe (Bool, ControllersWithId))
            pushfn (_, Drag2 (fromX, fromY) (toX, toY) _ _ _) = do
              let
                r = CTagBox :=> (Identity $ CBox {
                    _cBox_deltaBox = DeltaLBox 0 $ V2 (toX-fromX) (toY-fromY)
                  })
              wasManip' <- sample $ current wasManip
              return . Just $ (wasManip', IM.singleton _mBox_target r) where
          return $ (never, push pushfn (cursorDragStateEv (Just CSBox) (Just Dragging) _manipulatorWidgetConfig_drag))
        _ -> undefined
      return w


  dynWidget :: Dynamic t (VtyWidget t m (Event t (Bool, ControllersWithId)))
    <- holdDyn (return never) (fmap (mapfn . snd) selectionChangedEv)
  modifyEv :: Event t (Bool, ControllersWithId)
    <- networkView dynWidget >>= switchHold never

-}
  return
    ManipulatorWidget {
      -- TODO
      _manipulatorWidget_modify = modifyEv
      -- TODO
      , _manipulatorWidget_manipulating = undefined
    }
