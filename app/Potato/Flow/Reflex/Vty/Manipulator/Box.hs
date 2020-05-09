{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Manipulator.Box (
  BoxHandleType(..)
  , BoxManipWidgetConfig(..)
  , makeBoxManipWidget
) where

import           Relude


import           Potato.Flow
import           Potato.Flow.Reflex.Vty.CanvasPane
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
import           Data.Tuple.Extra

import qualified Graphics.Vty                              as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

data BoxHandleType = BH_TL | BH_TR | BH_BL | BH_BR | BH_T | BH_B | BH_L | BH_R | BH_A deriving (Show, Eq, Enum)

manipChar :: BoxHandleType -> Maybe Char
manipChar BH_TL = Just '╝'
manipChar BH_TR = Just '╚'
manipChar BH_BL = Just '╗'
manipChar BH_BR = Just '╔'
manipChar BH_T  = Just '═'
manipChar BH_B  = Just '═'
manipChar BH_L  = Just '║'
manipChar BH_R  = Just '║'
manipChar BH_A  = Just '$'


--let brBeh = ffor2 _manipulatorWidgetConfig_panPos (current lBoxDyn) (makeHandleBox bht)

makeHandleBox ::
  BoxHandleType
  -> (Int, Int) -- ^ canvas pan position
  -> LBox -- ^ box being manipulated
  -> LBox
makeHandleBox bht (px, py) (LBox (V2 x y) (V2 w h)) = case bht of
  BH_BR -> LBox (V2 r b) (V2 1 1)
  BH_TL -> LBox (V2 l t) (V2 1 1)
  BH_TR -> LBox (V2 r t) (V2 1 1)
  BH_BL -> LBox (V2 l b) (V2 1 1)
  BH_A  -> LBox (V2 (l+1) (t+1)) (V2 w h)
  _     -> error "not supported yet"
  where
    l = x+px-1
    t = y+py-1
    r = x+px+w
    b = y+py+h


--Just $ (,) ms $ Left $ IM.singleton _mBox_target $ CTagBox :=> (Identity $ CBox {
--  _cBox_deltaBox = makeDeltaBox bht (dx, dy)
--})

makeDeltaBox :: BoxHandleType -> (Int, Int) -> DeltaLBox
makeDeltaBox bht (dx,dy) = case bht of
  BH_BR -> DeltaLBox 0 $ V2 dx dy
  BH_TL -> DeltaLBox (V2 dx dy) (V2 (-dx) (-dy))
  BH_TR -> DeltaLBox (V2 0 dy) (V2 dx (-dy))
  BH_BL -> DeltaLBox (V2 dx 0) (V2 (-dx) dy)
  BH_T  -> DeltaLBox (V2 0 dy) (V2 0 (-dy))
  BH_B  -> DeltaLBox 0 (V2 0 dy)
  BH_L  -> DeltaLBox (V2 dx 0) (V2 (-dx) 0)
  BH_R  -> DeltaLBox 0 (V2 dx 0)
  BH_A  -> DeltaLBox (V2 dx dy) (V2 0 0)


data BoxManipWidgetConfig t = BoxManipWidgetConfig {

  -- These two are very timing dependent :(
  -- TODO is there some way to do this with toggle dyns or something instead?
  _boxManipWidgetConfig_wasLastModifyAdd :: Behavior t (Maybe Int)
  , _boxManipWidgetConfig_isNewElt       :: Behavior t Bool

  -- TODO probably better if you somehow attach above things to this, then use this to create Dynamic that tracks what type of operation we need
  , _boxManipWidgetConfig_updated        :: Event t (Bool, MBox)

  , _boxManipWidgetConfig_drag           :: Event t ((Int,Int), Drag2)
  , _boxManipWidgetConfig_panPos         :: Behavior t (Int, Int)
  , _boxManipWidgetConfig_pfctx          :: PFWidgetCtx t
}

makeBoxManipWidget :: forall t m. (MonadWidget t m)
  => BoxManipWidgetConfig t
  -> VtyWidget t m (ManipWidget t m)
makeBoxManipWidget BoxManipWidgetConfig {..} = mdo
  let
    boxManip_selectedEv = _boxManipWidgetConfig_updated
    mBoxEv = fmap snd boxManip_selectedEv
    lBoxEv = fmap _mBox_box mBoxEv


  -- TODO fmap canonicalLBox_from_lBox
  mBoxDyn <- holdDyn Nothing (fmap Just mBoxEv)
  -- TODO make this into a maybe and fmap _mBox_box mBoxDyn
  lBoxDyn <- holdDyn (LBox 0 0) lBoxEv


  let
    boxManip :: ManipWidget t m
    boxManip = do

      let
        handleTypes = [BH_BR, BH_TL, BH_TR, BH_BL, BH_A]
      handles <- forM handleTypes $ \bht -> do
        let handleBoxBeh = ffor2 _boxManipWidgetConfig_panPos (current lBoxDyn) (makeHandleBox bht)
        holdHandle $ HandleWidgetConfig {
            _handleWidgetConfig_pfctx = _boxManipWidgetConfig_pfctx
            , _handleWidgetConfig_box = handleBoxBeh
            , _handleWidgetConfig_graphic = constant $ manipChar bht
            , _handleWidgetConfig_dragEv = _boxManipWidgetConfig_drag
            , _handleWidgetConfig_forceDrag = if bht == BH_BR then _boxManipWidgetConfig_isNewElt else constant False
          }
      let
        handleDragEv = leftmost $ fmap (\(bht, h) -> fmap (\x -> (bht,x)) $ _handleWidget_dragged h) $ zip handleTypes handles
        didCaptureInput = leftmost $ fmap _handleWidget_didCaptureInput handles

      vLayoutPad 3 $ text $ fmap show _boxManipWidgetConfig_isNewElt
      vLayoutPad 4 $ debugStream $ [
        never
        --, fmapLabelShow "dragging" $ _manipulatorWidgetConfig_drag
        --, fmapLabelShow "drag" $ _handleWidget_dragged brHandle
        --, fmapLabelShow "modify" modifyEv
        ] -- <> map (\(x,h) -> fmapLabelShow (show x) (_handleWidget_dragged h)) (zip handleTypes handles)


      let
        pushfn :: (BoxHandleType, (ManipState, (Int, Int))) -> PushM t (Maybe (ManipState, Either ControllersWithId (LayerPos, SEltLabel)))
        pushfn (bht, (ms, (dx, dy))) = do
          mmbox <- sample . current $ mBoxDyn
          mremakelp <- sample _boxManipWidgetConfig_wasLastModifyAdd

          return $ case mmbox of
            Nothing -> Nothing
            Just MBox {..} -> case mremakelp of
              -- TODO somewhere along the way, this code path stopped being used :(
              -- TODO lBox_from_canonicalLBox
              Just lp -> assert (ms == ManipStart && bht == BH_BR) $ Just $ (,) Manipulating $ Right $
                (lp, SEltLabel "<box>" $ SEltBox $ SBox (LBox (_lBox_ul _mBox_box) (V2 dx dy)) def)
              -- TODO deltaLBox_via_canonicalLBox
              Nothing -> Just $ (,) ms $ Left $ IM.singleton _mBox_target $ CTagBox :=> (Identity $ CBox {
                  _cBox_deltaBox = makeDeltaBox bht (dx, dy)
                })

      return (push pushfn handleDragEv, didCaptureInput)

  return boxManip
