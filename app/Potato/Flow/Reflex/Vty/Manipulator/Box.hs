{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Manipulator.Box (
  BoxHandleType(..)
  , BoxManipWidgetConfig(..)
  , makeBoxManipWidget
) where

import           Relude


import           Potato.Flow
import           Potato.Flow.Reflex.Vty.Manipulator.Handle
import           Potato.Flow.Reflex.Vty.Manipulator.Types
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Flow.Reflex.Vty.Tools
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

-- returns pan position at start of drag and dragging info filtered for tool/drag state
toolDragStateEv :: (Reflex t)
  => Maybe Tool -- ^ tool state to select for
  -> Maybe DragState
  -> Event t (Tool, Drag2) -- ^ ((tool, panPos), drag)
  -> Event t Drag2
toolDragStateEv c' d' dragEv = r where
  fmapMaybeFn (c,d) = if maybe True (_drag2_state d  ==) d' && maybe True (c ==) c'
    then Just d
    else Nothing
  r = fmapMaybe fmapMaybeFn dragEv


data BoxHandleType = BH_TL | BH_TR | BH_BL | BH_BR | BH_T | BH_B | BH_L | BH_R | BH_A deriving (Show, Eq, Enum)

manipChar :: BoxHandleType -> Maybe Char
-- TODO Switch these all to something else since corners will flip sides
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
  -> Maybe LBox -- ^ box being manipulated
  -> Maybe LBox
makeHandleBox bht (px, py) mlbox = case mlbox of
  Nothing -> Nothing
  Just (LBox (V2 x y) (V2 w h)) -> Just $ case bht of
    BH_BR -> LBox (V2 r b) (V2 1 1)
    BH_TL -> LBox (V2 l t) (V2 1 1)
    BH_TR -> LBox (V2 r t) (V2 1 1)
    BH_BL -> LBox (V2 l b) (V2 1 1)
    BH_A  -> clbox
    _     -> error "not supported yet"
    where
      CanonicalLBox _ _ clbox = canonicalLBox_from_lBox $ LBox (V2 (x+px) (y+py)) (V2 w h)
      nudgex = if w < 0 then 1 else 0
      nudgey = if h < 0 then 1 else 0
      l = x+px-1 + nudgex
      t = y+py-1 + nudgey
      r = x+px+w - nudgex
      b = y+py+h - nudgey


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

  _boxManipWidgetConfig_updated        :: Event t MBox
  , _boxManipWidgetConfig_tool         :: Behavior t Tool
  , _boxManipWidgetConfig_drag         :: Event t (Tool, Drag2)
  , _boxManipWidgetConfig_panPos       :: Behavior t (Int, Int)
  , _boxManipWidgetConfig_selectionPos :: Behavior t LayerPos
  , _boxManipWidgetConfig_pfctx        :: PFWidgetCtx t
}

makeBoxManipWidget :: forall t m. (MonadWidget t m)
  => BoxManipWidgetConfig t
  -> VtyWidget t m (ManipWidget t m)
makeBoxManipWidget BoxManipWidgetConfig {..} = do
  -- ::created persistent dynamics (do not get recreated each time manipulator type changes)::
  let
    handleTypes = [BH_BR, BH_TL, BH_TR, BH_BL, BH_A]
  mBoxDyn <- holdDyn Nothing
    $ fmap Just _boxManipWidgetConfig_updated


  return $ mdo
    let
      -- this works for what we need it for, but semantically is kind of wrong
      newEltFinalizedEv = toolDragStateEv Nothing (Just DragEnd) _boxManipWidgetConfig_drag $> Right ()

      newEltDyn_foldfn :: Either (ManipState, Either ControllersWithId (LayerPos, SEltLabel)) () -> Bool -> Bool
      newEltDyn_foldfn (Left (ManipEnd, _)) _ = False
      newEltDyn_foldfn (Left (_, Right _)) _  = True
      newEltDyn_foldfn _ _                    = False
    newEltDyn <- foldDyn newEltDyn_foldfn False $ leftmost [
      newEltFinalizedEv
      , fmap Left modifyOrCreateEv
      , fmap Left newBoxEv
      -- this does not work becaues if we undo first hence this happens one frame after newBoxEv/modifyOrCreateEv
      --, _boxManipWidgetConfig_updated $> Right ()
      ]

    handles <- forM handleTypes $ \bht -> do
      let mHandleBoxBeh = ffor2 _boxManipWidgetConfig_panPos (current (_mBox_box <<$>> mBoxDyn)) (makeHandleBox bht)
      holdHandle $ HandleWidgetConfig {
          _handleWidgetConfig_pfctx = _boxManipWidgetConfig_pfctx
          , _handleWidgetConfig_mbox = mHandleBoxBeh
          , _handleWidgetConfig_graphic = constant $ manipChar bht
          , _handleWidgetConfig_dragEv = difference (toolDragStateEv Nothing Nothing _boxManipWidgetConfig_drag) newBoxEv
          , _handleWidgetConfig_forceDrag = if bht == BH_BR then current newEltDyn else constant False
        }
    let
      handleDragEv = leftmost $ fmap (\(bht, h) -> fmap (\x -> (bht,x)) $ _handleWidget_dragged h) $ zip handleTypes handles
      didCaptureInput = leftmost $ (newBoxEv $> ()) : fmap _handleWidget_didCaptureInput handles

    vLayoutPad 3 $ debugStreamBeh $ [ fmapLabelShow "newElt" (current newEltDyn) ]

    vLayoutPad 4 $ debugStream $ [
      never
      , fmapLabelShow "box" $ _boxManipWidgetConfig_updated
      , fmapLabelShow "drag" $ _boxManipWidgetConfig_drag
      , fmapLabelShow "moc" $ modifyOrCreateEv
      ] <> map (\(x,h) -> fmapLabelShow (show x) (_handleWidget_dragged h)) (zip handleTypes handles)

    -- ::create new events::
    let
      boxPushFn (Drag2 (fromX, fromY) _ _ _ _) = do
        (px, py) <- sample _boxManipWidgetConfig_panPos
        pos <- (+1) <$> sample _boxManipWidgetConfig_selectionPos
        return $ (pos, SEltLabel "<box>" $ SEltBox $ SBox (LBox (V2 (fromX-px) (fromY-py)) (V2 0 0)) def)
      newBoxEv' = pushAlways boxPushFn $ toolDragStateEv (Just TBox) (Just DragStart) _boxManipWidgetConfig_drag
      newBoxEv = fmap (\x -> (ManipStart, Right x)) newBoxEv'

    -- ::handle events::
    let
      pushfn :: (BoxHandleType, (ManipState, (Int, Int))) -> PushM t (Maybe (ManipState, Either ControllersWithId (LayerPos, SEltLabel)))
      pushfn (bht, (ms, (dx, dy))) = do
        mmbox <- sample . current $ mBoxDyn
        newElt <- sample . current $ newEltDyn
        remakelp <- sample _boxManipWidgetConfig_selectionPos

        return $ case mmbox of
          Nothing -> Nothing
          Just MBox {..} -> if newElt
            then assert (ms == ManipStart && bht == BH_BR) $ Just $ (,) Manipulating $ Right $
              (remakelp, SEltLabel "<box>" $ SEltBox $ SBox (LBox (_lBox_ul _mBox_box) (V2 dx dy)) def)
            else Just $ (,) ms $ Left $ IM.singleton _mBox_target $ CTagBox :=> (Identity $ CBox {
                _cBox_deltaBox = makeDeltaBox bht (dx, dy)
              })
      modifyOrCreateEv :: Event t (ManipState, Either ControllersWithId (LayerPos, SEltLabel))
      modifyOrCreateEv = push pushfn handleDragEv
    return (leftmostassert "create box" [modifyOrCreateEv, newBoxEv], didCaptureInput)
