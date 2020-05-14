{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Manipulator.BoundingBox (
  BoundingBoxManipWidgetConfig(..)
  , makeBoundingBoxManipWidget
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

import           Control.Exception
import           Control.Lens                              (over, _1, _2)
import           Control.Monad.Fix
import           Data.Dependent.Sum                        (DSum ((:=>)))
import qualified Data.IntMap.Strict                        as IM
import qualified Data.List.NonEmpty                        as NE
import           Data.These
import           Data.Tuple.Extra

import qualified Graphics.Vty                              as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty


union_LBoxes :: NonEmpty LBox -> LBox
union_LBoxes (x:|xs) = foldl' union_LBox x xs


data BoundingBoxManipWidgetConfig t = BoundingBoxManipWidgetConfig {
  _boundingBoxManipWidgetConfig_updated  :: Event t MBoundingBox
  , _boundingBoxManipWidgetConfig_drag   :: Event t (Tool, Drag2)
  , _boundingBoxManipWidgetConfig_panPos :: Behavior t (Int, Int)
  , _boundingBoxManipWidgetConfig_pfctx  :: PFWidgetCtx t
  , _boundingBoxManipWidgetConfig_cancel :: Event t ()
}

makeBoundingBoxManipWidget :: forall t m. (MonadWidget t m)
  => BoundingBoxManipWidgetConfig t
  -> VtyWidget t m (ManipWidget t m)
makeBoundingBoxManipWidget BoundingBoxManipWidgetConfig {..} = do
  -- ::created persistent dynamics (do not get recreated each time manipulator type changes)::
  let
    handleTypes = [BH_BR, BH_TL, BH_TR, BH_BL, BH_A]
  mBoundingBoxDyn <- holdDyn Nothing $ fmap Just _boundingBoxManipWidgetConfig_updated

  return $ mdo
    let
      modifyFinalizedEv = leftmost
        [ _boundingBoxManipWidgetConfig_cancel $> Right ()
        , toolDragStateEv (Just TSelect) (Just DragEnd) _boundingBoxManipWidgetConfig_drag $> Right ()]

    handles <- forM handleTypes $ \bht -> do
      let mHandleBoxBeh = ffor2 _boundingBoxManipWidgetConfig_panPos (current (union_LBoxes . fmap snd . _mBoundingBox_bounded_targets <<$>> mBoundingBoxDyn)) (makeHandleBox bht)

      holdHandle $ HandleWidgetConfig {
          _handleWidgetConfig_pfctx = _boundingBoxManipWidgetConfig_pfctx
          , _handleWidgetConfig_mbox = mHandleBoxBeh
          , _handleWidgetConfig_graphic = constant $ manipChar bht
          , _handleWidgetConfig_dragEv = toolDragStateEv (Just TSelect) Nothing _boundingBoxManipWidgetConfig_drag
          , _handleWidgetConfig_forceDrag = constant False
          , _handleWidgetConfig_cancel = _boundingBoxManipWidgetConfig_cancel
        }
    let
      handleDragEv = leftmost $ fmap (\(bht, h) -> fmap (\x -> (bht,x)) $ _handleWidget_dragged h) $ zip handleTypes handles
      didCaptureInput = leftmost $ fmap _handleWidget_didCaptureInput handles

    vLayoutPad 4 $ debugStream $ [
      never
      --, fmapLabelShow "box" $ _boxManipWidgetConfig_updated
      --, fmapLabelShow "drag" $ _boxManipWidgetConfig_drag
      --, fmapLabelShow "moc" $ modifyOrCreateEv
      ] -- <> map (\(x,h) -> fmapLabelShow (show x) (_handleWidget_dragged h)) (zip handleTypes handles)


    -- ::handle events::
    let
      pushfn :: (BoxHandleType, (ManipState, (Int, Int))) -> PushM t (Maybe (ManipState, ControllersWithId))
      pushfn (bht, (ms, (dx, dy))) = do
        mmbox <- sample . current $ mBoundingBoxDyn

        return $ case mmbox of
          Nothing -> Nothing
          Just MBoundingBox {..} -> Just $ (,) ms $ IM.fromList . NE.toList $ flip fmap _mBoundingBox_bounded_targets
            -- TODO do relative scaling on orig
            $ \(rid, orig) -> (rid, CTagBoundingBox :=> (Identity $ CBoundingBox {
                _cBoundingBox_deltaBox = makeDeltaBox bht (dx, dy)
              }))
      modifyOrCreateEv :: Event t (ManipState, Either ControllersWithId (LayerPos, SEltLabel))
      modifyOrCreateEv = fmap (over _2 Left) $ push pushfn handleDragEv

    return
      ManipOutput {
        _manipOutput_manipulate = modifyOrCreateEv
        , _manipOutput_consumedInput = didCaptureInput
      }
