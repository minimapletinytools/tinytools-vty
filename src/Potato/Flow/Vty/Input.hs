
module Potato.Flow.Vty.Input (
  convertModifiers
  , convertKey
  , convertButton
  , makeLMouseDataInputEv
) where
import           Relude

import           Potato.Flow

import qualified Graphics.Vty              as V
import           Reflex
import           Reflex.Vty

import           Control.Monad.Fix

convertModifiers :: [V.Modifier] -> [KeyModifier]
convertModifiers = fmap $ \case
  V.MShift -> KeyModifier_Shift
  V.MCtrl  -> KeyModifier_Ctrl
  V.MMeta  -> KeyModifier_Ctrl
  V.MAlt ->  KeyModifier_Alt

convertKey :: V.Key -> Maybe KeyboardKey
convertKey = \case
  V.KEsc   -> Just KeyboardKey_Esc
  V.KChar c -> Just $   KeyboardKey_Char c
  V.KBS   -> Just KeyboardKey_Backspace
  V.KEnter   -> Just KeyboardKey_Return
  V.KLeft   -> Just KeyboardKey_Left
  V.KRight   -> Just KeyboardKey_Right
  V.KUp   -> Just KeyboardKey_Up
  V.KDown   -> Just KeyboardKey_Down
  V.KUpLeft   -> Nothing
  V.KUpRight -> Nothing
  V.KDownLeft -> Nothing
  V.KDownRight -> Nothing
  V.KCenter -> Nothing
  V.KFun _ -> Nothing
  V.KBackTab -> Nothing
  V.KPrtScr -> Nothing
  V.KPause -> Nothing
  V.KIns -> Nothing
  V.KHome -> Just KeyboardKey_Home
  V.KDel -> Just KeyboardKey_Delete
  V.KEnd -> Just KeyboardKey_End
  V.KBegin -> Nothing
  V.KMenu -> Nothing
  -- disabled for now cuz I use for debugging
  -- TODO enable
  --V.KPageUp -> Just KeyboardKey_PageUp
  --V.KPageDown -> Just KeyboardKey_PageDown
  _ -> Nothing


convertButton :: V.Button -> Maybe MouseButton
convertButton = \case
  V.BLeft -> Just MouseButton_Left
  V.BMiddle -> Just MouseButton_Middle
  V.BRight -> Just MouseButton_Right
  V.BScrollUp -> Nothing
  V.BScrollDown -> Nothing

tupleToXY :: (Int, Int) -> XY
tupleToXY (x,y) = V2 x y


makeLMouseDataInputEv
  :: (Reflex t, MonadFix m, MonadHold t m, HasInput t m)
  => Dynamic t (Int, Int)
  -> Bool
  -> m (Event t LMouseData)
makeLMouseDataInputEv offsetDyn isLayerMouse = do
  -- NOTE, must report both mouse down and up for any given drag or things will break
  -- button/mods is always the same button as mouse down, even if it changes during a drag
  inp <- input

  let
    mouseDownEv = fforMaybe inp $ \case
      V.EvMouseDown _ _ b mods -> Just (b, mods)
      _ -> Nothing
    -- tracks if last event was a mouse up
    mouseUpEv = fforMaybe inp $ \case
      V.EvMouseUp _ _ _ -> Just True
      V.EvMouseDown _ _ _ _ -> Just False
      _ -> Nothing
    mouseDownFoldFn (True, x) _  = x -- only updated button/mods just after a mouse up
    mouseDownFoldFn (False, _) x = x
  mouseUpDyn <- holdDyn True mouseUpEv
  mouseDownDyn <- foldDyn mouseDownFoldFn (V.BLeft,[]) (attach (current mouseUpDyn) mouseDownEv)

  return $ fforMaybe (attach (current ((,) <$> mouseDownDyn <*> offsetDyn)) inp) $ \case
    (_, V.EvMouseDown _ _ V.BScrollUp _) -> Nothing
    (_, V.EvMouseDown _ _ V.BScrollDown _) -> Nothing
    ((_,offset), V.EvMouseDown x y b mods) -> case convertButton b of
      Nothing -> Nothing
      Just b' -> Just $ LMouseData {
        _lMouseData_position       = (V2 x y) + tupleToXY offset
        , _lMouseData_isRelease    = False
        , _lMouseData_button       = b'
        , _lMouseData_modifiers    = convertModifiers mods
        , _lMouseData_isLayerMouse = isLayerMouse
      }
    (((b,mods),offset), V.EvMouseUp x y _) -> case convertButton b of
      Nothing -> Nothing
      Just b' -> Just $ LMouseData {
        _lMouseData_position       = (V2 x y) + tupleToXY offset
        , _lMouseData_isRelease    = True
        , _lMouseData_button       = b'
        , _lMouseData_modifiers    = convertModifiers mods
        , _lMouseData_isLayerMouse = isLayerMouse
      }
    _ -> Nothing
