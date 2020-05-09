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

holdManipulatorWidget :: forall t m. (MonadWidget t m)
  => ManipulatorWidgetConfig t
  -> VtyWidget t m (ManipulatorWidget t)
holdManipulatorWidget ManipulatorWidgetConfig {..} = mdo

  -- ::track if we are manipulating::
  -- TODO probably can delete, handles track this themselves
  -- Tracks whether we're manipulating. This is needed so that we don't undo the first manipulation event.
  --let dragEnd = cursorDragStateEv Nothing (Just DragEnd) _manipulatorWidgetConfig_drag
  --bManipulating <- return . current
  --  =<< (holdDyn False $ leftmost [dragEnd $> False, manipulateEv $> True])

  -- ::collected various change events::
  let selectionChangedEv = updated _manipulatorWigetConfig_selected
  -- tracks whether an elements was newly created or not
  -- NOTE very timing dependent
  newEltBeh <- hold False (fmap fst selectionChangedEv)
  -- LayerPos this is needed to recreate a new element after undoing it
  selectionLayerPos <- hold (-1)
    $ fmap (maybe (-1) (\(_,lp,_) -> lp))
    $ fmap (viaNonEmpty NE.head)
    $ fmap snd selectionChangedEv
  let
    wasLastModifyAdd = ffor3 (current isManipulatingDyn) newEltBeh selectionLayerPos (\m n lp -> if m && n then Just lp else Nothing)

  -- TODO mov ethis into Selection
  -- ::convert to Manipulator EventSelector::
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

  -- ::create the manipulators::
  boxManip <- makeBoxManipWidget  BoxManipWidgetConfig {
      _boxManipWidgetConfig_wasLastModifyAdd = wasLastModifyAdd
      , _boxManipWidgetConfig_isNewElt = newEltBeh
      , _boxManipWidgetConfig_updated = selectManip MTagBox
      -- TODO this only needs CSSelecting for modify and CSBox for creating new boxes
      , _boxManipWidgetConfig_drag  = cursorDragStateEv Nothing Nothing _manipulatorWidgetConfig_drag
      , _boxManipWidgetConfig_panPos = _manipulatorWidgetConfig_panPos
      , _boxManipWidgetConfig_pfctx = _manipulatorWigetConfig_pfctx
    }

  -- ::networkHold the correct manipulator::
  let
    finalManip :: Event t (VtyWidget t m (ManipOutput t))
    finalManip = ffor (updated dynManipSelTypeChange) $ \case
      MSTBox -> boxManip
      _ -> return (never, never)
  -- NOTE the 'networkHold' here doesn't seem to play well with other places where I use 'runWithAdjust'
  -- thus, we use 'dynManipSelTypeChange' above instead to limit the number of times the widget changes (even if nothing actually changes)
  -- CORRECTION, this is probbaly just because dynamics inside manip widgets are getting recreated by networkHold and less related to runWithAdjust conflicts
  -- still better to have fewer network updates like this.
  manipWidget :: Dynamic t (ManipOutput t)
    <- networkHold (return (never, never)) finalManip

  -- ::collect output events::
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
    , fmapLabelShow "isManip" $ updated isManipulatingDyn
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
