{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Manipulator (
  ManipulatorWidgetConfig(..)
  , ManipulatorWidget(..)
  , holdManipulatorWidget
) where

import           Relude


import           Potato.Flow
import           Potato.Flow.Reflex.Vty.CanvasPane
import           Potato.Flow.Reflex.Vty.Manipulator.Box
import           Potato.Flow.Reflex.Vty.Manipulator.Handle
import           Potato.Flow.Reflex.Vty.Manipulator.Types
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget

import           Control.Exception
import           Control.Lens                              (over, _1)
import           Control.Monad.Fix
import           Data.Dependent.Sum                        (DSum ((:=>)))
import qualified Data.IntMap.Strict                        as IM
import qualified Data.List.NonEmpty                        as NE
import           Data.These

import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty


maybeLeft :: Either a b -> Maybe a
maybeLeft (Left a) = Just a
maybeLeft _        = Nothing

maybeRight :: Either a b -> Maybe b
maybeRight (Right b) = Just b
maybeRight _         = Nothing

data ManipulatorWidgetConfig t = ManipulatorWidgetConfig {
  _manipulatorWigetConfig_pfctx :: PFWidgetCtx t
  , _manipulatorWigetConfig_selected  :: Dynamic t (Bool, [SuperSEltLabel])
  , _manipulatorWidgetConfig_panPos :: Behavior t (Int, Int)
  , _manipulatorWidgetConfig_drag   :: Event t ((CursorState, (Int,Int)), Drag2)
}

data ManipulatorWidget t = ManipulatorWidget {
  _manipulatorWidget_modify            :: Event t (Bool, ControllersWithId) -- ^ first param is whether we should undo previous action or not
  , _manipulatorWidget_add             :: Event t (Bool, (LayerPos, SEltLabel)) -- ^ first param is whether we should undo previous action or not
  --, _manipulatorWidget_manipulating :: Dynamic t Bool
  , _manipulatorWidget_didCaptureMouse :: Event t ()
}

holdManipulatorWidget :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => ManipulatorWidgetConfig t
  -> VtyWidget t m (ManipulatorWidget t)
holdManipulatorWidget ManipulatorWidgetConfig {..} = mdo

  -- TODO probably can delete, handles track this themselves
  -- Tracks whether we're manipulating. This is needed so that we don't undo the first manipulation event.
  let dragEnd = cursorDragStateEv Nothing (Just DragEnd) _manipulatorWidgetConfig_drag
  bManipulating <- return . current
    =<< (holdDyn False $ leftmost [dragEnd $> False, manipulateEv $> True])

  let selectionChangedEv = updated _manipulatorWigetConfig_selected
  -- tracks whether an elements was newly created or not
  -- NOTE very timing dependent
  newEltBeh <- hold False (fmap fst selectionChangedEv)
  -- this is needed to recreate a new element after undoing it
  selectionLayerPos <- hold (-1)
    $ fmap (maybe (-1) (\(_,lp,_) -> lp))
    $ fmap (viaNonEmpty NE.head)
    $ fmap snd selectionChangedEv

  let
    wasLastModifyAdd = ffor3 (current isManipulatingDyn) newEltBeh selectionLayerPos (\m n lp -> if m && n then Just lp else Nothing)


  dynManipulator <- toManipulator $ fmap snd selectionChangedEv
  -- see comments on 'manipWidget'
  dynManipSelTypeChange' <- holdDyn MSTNone $ ffor (updated dynManipulator) $ \case
    (MTagBox :=> _) -> MSTBox
    (MTagLine :=> _) -> MSTLine
    (MTagText :=> _) -> MSTText
    (MTagBoundingBox :=> _) -> MSTBBox
    _ -> MSTNone
  dynManipSelTypeChange <- holdUniqDyn dynManipSelTypeChange'

  let
    selectManip :: MTag a -> Event t (Bool, a)
    selectManip mtag = r where
      selectManip' = select (fanDSum (updated dynManipulator)) mtag
      -- NOTE this completely negates the performance of using select/fan, you need to stick the tuple inside the DSum to do this right
      alignfn (These ns m) = Just (ns, m)
      alignfn _            = Nothing
      r = alignEventWithMaybe alignfn (fmap fst $ selectionChangedEv) selectManip'


  -- TODO you should prob split into functions...
  -- BOX MANIPULATOR
  let
    boxManip_selectedEv = selectManip MTagBox
    boxManip_dmBox = fmap snd boxManip_selectedEv
    boxManip_dlbox = fmap _mBox_box boxManip_dmBox
  boxManip_dynBox <- holdDyn Nothing (fmap Just boxManip_dmBox)
  boxManip_dlboxDyn <- holdDyn (LBox 0 0) boxManip_dlbox

  let
    boxManip :: VtyWidget t m (ManipOutput t)
    boxManip = do
      let brBeh = ffor2 _manipulatorWidgetConfig_panPos (current boxManip_dlboxDyn) (\(px, py) (LBox (V2 x y) (V2 w h)) -> (x+px+w, y+py+h))
      brHandle <- holdHandle $ HandleWidgetConfig {
          _handleWidgetConfig_pfctx = _manipulatorWigetConfig_pfctx
          , _handleWidgetConfig_position = brBeh
          , _handleWidgetConfig_graphic = constant '┌'
          -- TODO only pass on if our cursor type is CSSelecting (but make sure after creating a new elt, our cursor is switched to CSSelecting)
          , _handleWidgetConfig_dragEv = cursorDragStateEv Nothing Nothing _manipulatorWidgetConfig_drag
          , _handleWidgetConfig_forceDrag = newEltBeh
        }
      let
        brHandleDragEv = fmap (\x -> (BH_BR, x)) $ _handleWidget_dragged brHandle

      vLayoutPad 4 $ debugStream [
        never
        --, fmapLabelShow "dragging" $ _manipulatorWidgetConfig_drag
        --, fmapLabelShow "drag" $ _handleWidget_dragged brHandle
        --, fmapLabelShow "modify" modifyEv
        ]


      let
        pushfn :: (BoxHandleType, (ManipState, (Int, Int))) -> PushM t (Maybe (ManipState, Either ControllersWithId (LayerPos, SEltLabel)))
        pushfn (bht, (ms, (dx, dy))) = do
          mmbox <- sample . current $ boxManip_dynBox
          mremakelp <- sample wasLastModifyAdd

          return $ case mmbox of
            Nothing -> Nothing
            Just MBox {..} -> case mremakelp of
              Just lp -> assert (ms == ManipStart) $ Just $ (,) Manipulating $ Right $
                (lp, SEltLabel "<box>" $ SEltBox $ SBox (LBox (_lBox_ul _mBox_box) (V2 dx dy)) def)
              Nothing -> Just $ (,) ms $ Left $ IM.singleton _mBox_target $ CTagBox :=> (Identity $ CBox {
                  _cBox_deltaBox = DeltaLBox 0 $ V2 dx dy
                })


      return (push pushfn brHandleDragEv, _handleWidget_didCaptureInput brHandle)


    finalManip :: Event t (VtyWidget t m (ManipOutput t))
    finalManip = ffor (updated dynManipSelTypeChange) $ \case
      MSTBox -> boxManip
      _ -> return (never, never)
    -- TODO the rest of them


  -- NOTE the 'networkHold' here doesn't seem to play well with other places where I use 'runWithAdjust'
  -- thus, we use 'dynManipSelTypeChange' above instead to limit the number of times the widget changes (even if nothing actually changes)
  -- CORRECTION, this is probbaly just because dynamics inside manip widgets are getting recreated by networkHold and less related to runWithAdjust conflicts
  -- still better to have fewer network updates like this.
  manipWidget :: Dynamic t (ManipOutput t)
    <- networkHold (return (never, never)) finalManip

  let
    rawManipEv = switchDyn (fmap fst manipWidget)
    manipulateEv :: Event t (Bool, Either ControllersWithId (LayerPos, SEltLabel))
    manipulateEv = fmap (over _1 needUndoFirst) $ rawManipEv
    didCaptureMouseEv :: Event t ()
    didCaptureMouseEv = switchDyn (fmap snd manipWidget)

  isManipulatingDyn <- holdDyn False $ fmap isManipulating (fmap fst rawManipEv)

  debugStream [
    never
    , fmapLabelShow "manip" $ manipulateEv
    --, fmapLabelShow "dynManip" $ selectionChangedEv
    --, fmapLabelShow "dynManip" $ selectManip MTagBox
    --, fmapLabelShow "changes" $ _sEltLayerTree_changeView $ _pfo_layers $ _pFWidgetCtx_pfo _manipulatorWigetConfig_pfctx
    ]

  return
    ManipulatorWidget {
      _manipulatorWidget_modify = fmapMaybe (\(b,e) -> maybeLeft e >>= (\l -> return (b,l))) manipulateEv
      , _manipulatorWidget_add = fmapMaybe (\(b,e) -> maybeRight e >>= (\r -> return (b,r))) manipulateEv
      , _manipulatorWidget_didCaptureMouse = didCaptureMouseEv
    }
