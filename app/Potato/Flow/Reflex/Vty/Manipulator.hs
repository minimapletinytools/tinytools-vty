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


import           Control.Monad.Fix
import           Data.Dependent.Sum        (DSum ((:=>)))
import qualified Data.IntMap.Strict as IM

import           Reflex
import           Reflex.Network
import           Reflex.Vty





data ManipulatorWidgetConfig t = ManipulatorWidgetConfig {
  _manipulatorWigetConfig_selected  :: Dynamic t Selected
  , _manipulatorWidgetConfig_panPos :: Behavior t (Int, Int)
  -- TODO currently pane drag won't register release events off the pane, so we need to make sure to pipe in those release events
  , _manipulatorWidgetConfig_drag   :: Event t ((CursorState, (Int,Int)), Drag2)
  -- TODO
  --, _manipulatorWidgetConfig_cancel :: Event t ()
}

data ManipulatorWidget t = ManipulatorWidget {
  _manipulatorWidget_modify :: Event t (Bool, ControllersWithId) -- ^ first param is whether we should undo previous action or not
  , _manipulatorWidget_manipulating :: Dynamic t Bool
}

holdManipulatorWidget :: forall t m. (Reflex t, MonadHold t m, MonadFix m, NotReady t m, Adjustable t m, PostBuild t m)
  => ManipulatorWidgetConfig t
  -> VtyWidget t m (ManipulatorWidget t)
holdManipulatorWidget ManipulatorWidgetConfig {..} = do
  let
    selectionChangedEv = updated _manipulatorWigetConfig_selected
  dynManipulator <- toManipulator $ selectionChangedEv
  let
    -- recreate the manipulator each time the selection changes
    mapfn :: Selected -> VtyWidget t m (Event t (Bool, ControllersWithId))
    mapfn _ = mdo
      -- TODO hook up to didStart
      let
        -- TODO figure this out
        dragging = cursorDragStateEv (Just CSBox) (Just Dragging) _manipulatorWidgetConfig_drag
        dragEnd = cursorDragStateEv Nothing (Just DragEnd) _manipulatorWidgetConfig_drag
      wasManip <- holdDyn False $ leftmost [didStart $> True, dragging $> True, dragEnd $> False]

      manipulator <- sample $ current dynManipulator
        -- assumes manipulator is always referring to same SEltLabel
        -- TODO add assert to ensure it doesn't change
      (didStart, w) <- case manipulator of
        (MTagBox :=> Identity (MBox {..})) -> do
          let
            --LBox (LPoint (V2 x y)) (LSize (V2 w h)) = _mBox_box
            -- TODO draw 4 corner images
            -- create 8 drag events
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
    <- holdDyn (return never) (fmap mapfn selectionChangedEv)
  modifyEv :: Event t (Bool, ControllersWithId)
    <- networkView dynWidget >>= switchHold never
  return
    ManipulatorWidget {
      -- TODO
      _manipulatorWidget_modify = modifyEv
      -- TODO
      , _manipulatorWidget_manipulating = undefined
    }
