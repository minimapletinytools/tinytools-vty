{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Manipulator.Box (
  BoxHandleType(..)
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

data BoxHandleType = BH_TL | BH_TR | BH_BL | BH_BR | BH_T | BH_B | BH_L | BH_R deriving (Show, Eq, Enum)

--let brBeh = ffor2 _manipulatorWidgetConfig_panPos (current boxManip_dlboxDyn) (makeCornerHandlePos bht)

makeCornerHandlePos ::
  BoxHandleType
  -> (Int, Int) -- ^ canvas pan position
  -> LBox -- ^ box being manipulated
  -> (Int, Int)
makeCornerHandlePos bht (px, py) (LBox (V2 x y) (V2 w h)) = case bht of
  BH_BR -> (r, b)
  BH_TL -> (l, t)
  BH_TR -> (r, t)
  BH_BL -> (b, l)
  _     -> error "don't use this for non-corner handles"
  where
    l = x+px
    t = y+py
    r = x+px+w
    b = y+py+h


--Just $ (,) ms $ Left $ IM.singleton _mBox_target $ CTagBox :=> (Identity $ CBox {
--  _cBox_deltaBox = makeDeltaBox bht (dx, dy)
--})

makeDeltaBox :: BoxHandleType -> (Int, Int) -> DeltaLBox
makeDeltaBox bht (dx,dy) = case bht of
  BH_BR -> DeltaLBox 0 $ V2 dx dy
  BH_TL -> DeltaLBox (V2 dx dy) 0
  BH_TR -> DeltaLBox (V2 0 dy) (V2 dx 0)
  BH_BL -> DeltaLBox (V2 dx 0) (V2 0 dy)
  BH_T  -> DeltaLBox (V2 0 dy) 0
  BH_B  -> DeltaLBox 0 (V2 0 dy)
  BH_L  -> DeltaLBox (V2 dx 0) 0
  BH_R  -> DeltaLBox 0 (V2 dx 0)


data BoxManipWidgetConfig t = BoxManipWidgetConfig {
  _boxManipWidgetConfig_wasLastModifyAdd :: Behavior t (Maybe Int)
  , _boxManipWidgetConfig_updated        :: Event t (Bool, MBox)
  -- TODO prob don't need/want cursor state here
  , _boxManipWidgetConfig_drag   :: Event t ((CursorState, (Int,Int)), Drag2)
  , _boxManipWidgetConfig_panPos :: Behavior t (Int, Int)
  , _boxManipWidgetConfig_pfctx :: PFWidgetCtx t
}

makeBoxManipWidget :: forall t m. (MonadWidget t m)
  => BoxManipWidgetConfig t
  -> VtyWidget t m (ManipWidget t m)
makeBoxManipWidget BoxManipWidgetConfig {..} = mdo
  -- TODO you should prob split into functions...
  -- BOX MANIPULATOR
  let
    boxManip_selectedEv = _boxManipWidgetConfig_updated
    boxManip_dmBox = fmap snd boxManip_selectedEv
    boxManip_dlbox = fmap _mBox_box boxManip_dmBox
  boxManip_dynBox <- holdDyn Nothing (fmap Just boxManip_dmBox)
  boxManip_dlboxDyn <- holdDyn (LBox 0 0) boxManip_dlbox

  let
    boxManip :: ManipWidget t m
    boxManip = do
      let brBeh = ffor2 _boxManipWidgetConfig_panPos (current boxManip_dlboxDyn) (\(px, py) (LBox (V2 x y) (V2 w h)) -> (x+px+w, y+py+h))
      brHandle <- holdHandle $ HandleWidgetConfig {
          _handleWidgetConfig_pfctx = _boxManipWidgetConfig_pfctx
          , _handleWidgetConfig_position = brBeh
          , _handleWidgetConfig_graphic = constant '┌'
          -- TODO only pass on if our cursor type is CSSelecting (but make sure after creating a new elt, our cursor is switched to CSSelecting)
          , _handleWidgetConfig_dragEv = cursorDragStateEv Nothing Nothing _boxManipWidgetConfig_drag
          -- TODO make sure this works, add comment why it's ok
          , _handleWidgetConfig_forceDrag = fmap isJust _boxManipWidgetConfig_wasLastModifyAdd
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
          mremakelp <- sample _boxManipWidgetConfig_wasLastModifyAdd

          return $ case mmbox of
            Nothing -> Nothing
            Just MBox {..} -> case mremakelp of
              Just lp -> assert (ms == ManipStart) $ Just $ (,) Manipulating $ Right $
                (lp, SEltLabel "<box>" $ SEltBox $ SBox (LBox (_lBox_ul _mBox_box) (V2 dx dy)) def)
              Nothing -> Just $ (,) ms $ Left $ IM.singleton _mBox_target $ CTagBox :=> (Identity $ CBox {
                  _cBox_deltaBox = DeltaLBox 0 $ V2 dx dy
                })

      return (push pushfn brHandleDragEv, _handleWidget_didCaptureInput brHandle)

  return boxManip
