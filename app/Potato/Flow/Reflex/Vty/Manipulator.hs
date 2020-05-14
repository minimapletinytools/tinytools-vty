{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Manipulator (
  ManipulatorWidgetConfig(..)
  , ManipulatorWidget(..)
  , holdManipulatorWidget
) where

import           Relude


import           Potato.Flow
import           Potato.Flow.Reflex.Vty.Manipulator.Box
import           Potato.Flow.Reflex.Vty.Manipulator.Handle
import           Potato.Flow.Reflex.Vty.Manipulator.Types
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Flow.Reflex.Vty.Tools
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget

import           Control.Lens                              (over, _1)
import qualified Data.List.NonEmpty                        as NE

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
  _manipulatorWigetConfig_pfctx      :: PFWidgetCtx t
  , _manipulatorWigetConfig_selected :: Dynamic t (ManipSelectionType, [SuperSEltLabel])
  , _manipulatorWidgetConfig_panPos  :: Behavior t (Int, Int)
  , _manipulatorWidgetConfig_drag    :: Event t (Tool, Drag2)
  , _manipulatorWidgetConfig_tool    :: Dynamic t Tool
}

data ManipulatorWidget t = ManipulatorWidget {
  _manipulatorWidget_modify            :: Event t (Bool, ControllersWithId) -- ^ first param is whether we should undo previous action or not
  , _manipulatorWidget_add             :: Event t (Bool, (LayerPos, SEltLabel)) -- ^ first param is whether we should undo previous action or not
  --, _manipulatorWidget_manipulating :: Dynamic t Bool
  , _manipulatorWidget_didCaptureMouse :: Event t ()
  , _manipulatorWidget_undo            :: Event t ()
}

holdManipulatorWidget :: forall t m. (MonadWidget t m)
  => ManipulatorWidgetConfig t
  -> VtyWidget t m (ManipulatorWidget t)
holdManipulatorWidget ManipulatorWidgetConfig {..} = mdo

  -- ::collected various change events::
  let selectionChangedEv = updated _manipulatorWigetConfig_selected

  -- TODO make this position of last selected element
  selectionLayerPosBeh :: Behavior t LayerPos <- hold (-1)
    $ fmap (maybe (-1) (\(_,lp,_) -> lp))
    $ fmap (viaNonEmpty NE.head)
    $ fmap snd selectionChangedEv

  -- ::convert to Manipulator EventSelector::
  dynManipulator <- toManipulator $ fmap snd selectionChangedEv
  selectionTypeDyn <- holdDyn MSTNone $ fmap fst selectionChangedEv

  let
    selectManip :: MTag a -> Event t a
    selectManip mtag = select (fanDSum (updated dynManipulator)) mtag

  -- ::tools overwrite selection type::
  let
    manipulatorTypeDyn' = ffor2 _manipulatorWidgetConfig_tool selectionTypeDyn $ \tool selType -> case tool of
      TBox  -> MSTBox
      TLine -> MSTLine
      TText -> MSTText
      --TPan -> MSTNone
      _     -> selType
  manipulatorTypeDyn <- holdUniqDyn manipulatorTypeDyn'

  -- ::create the manipulators::
  boxManip <- makeBoxManipWidget  BoxManipWidgetConfig {
      _boxManipWidgetConfig_updated = selectManip MTagBox
      , _boxManipWidgetConfig_tool = current _manipulatorWidgetConfig_tool
      , _boxManipWidgetConfig_drag  = _manipulatorWidgetConfig_drag
      , _boxManipWidgetConfig_panPos = _manipulatorWidgetConfig_panPos
      , _boxManipWidgetConfig_pfctx = _manipulatorWigetConfig_pfctx
      , _boxManipWidgetConfig_selectionPos = selectionLayerPosBeh
      , _boxManipWidgetConfig_cancel = undoEv
    }

  -- ::networkHold the correct manipulator::
  let
    finalManip :: Event t (VtyWidget t m (ManipOutput t))
    finalManip = ffor (updated manipulatorTypeDyn) $ \case
      MSTBox -> boxManip
      _ -> return $ ManipOutput never never
  manipWidget :: Dynamic t (ManipOutput t)
    <- networkHold (return (ManipOutput never never)) finalManip

  -- ::collect output events::
  let
    rawManipEv = switchDyn (fmap _manipOutput_manipulate manipWidget)
    manipulateEv :: Event t (Bool, Either ControllersWithId (LayerPos, SEltLabel))
    manipulateEv = fmap (over _1 needUndoFirst) $ rawManipEv
    didCaptureMouseEv :: Event t ()
    didCaptureMouseEv = switchDyn (fmap _manipOutput_consumedInput manipWidget)

    isManipulatingDyn_foldfn :: Either () ManipState -> Bool -> Bool
    isManipulatingDyn_foldfn (Left _) _   = False
    isManipulatingDyn_foldfn (Right ms) _ = isManipulating ms
    isManipulatingDyn_foldfn _ _          = True
  isManipulatingDyn <- foldDyn isManipulatingDyn_foldfn False $ leftmostassert "isManipulatingDyn" [fmap (Right . fst) rawManipEv, fmap Left (_pFWidgetCtx_ev_cancel _manipulatorWigetConfig_pfctx)]

  let
    undoEv = gate (current isManipulatingDyn) (_pFWidgetCtx_ev_cancel _manipulatorWigetConfig_pfctx)

  debugStream [
    never
    --, fmapLabelShow "manip" $ manipulateEv
    --, fmapLabelShow "isManip" $ updated isManipulatingDyn
    --, fmapLabelShow "dynManip" $ selectionChangedEv
    --, fmapLabelShow "dynManip" $ selectManip MTagBox
    --, fmapLabelShow "changes" $ _sEltLayerTree_changeView $ _pfo_layers $ _pFWidgetCtx_pfo _manipulatorWigetConfig_pfctx
    ]

  return
    ManipulatorWidget {
      _manipulatorWidget_modify = fmapMaybe (\(b,e) -> maybeLeft e >>= (\l -> return (b,l))) manipulateEv
      , _manipulatorWidget_add = fmapMaybe (\(b,e) -> maybeRight e >>= (\r -> return (b,r))) manipulateEv
      , _manipulatorWidget_didCaptureMouse = didCaptureMouseEv
      , _manipulatorWidget_undo = undoEv
    }
