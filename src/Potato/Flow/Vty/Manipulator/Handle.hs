{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Manipulator.Handle (
  ManipState(..)
  , needUndoFirst
  , isManipulating
  , HandleWidgetConfig(..)
  , HandleWidget(..)
  , holdHandle
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Vty.Manipulator.Types
import           Potato.Flow.Vty.PFWidgetCtx
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget

import           Control.Monad.Fix

import qualified Graphics.Vty                             as V
import           Reflex
import           Reflex.Vty




needUndoFirst :: ManipState -> Bool
needUndoFirst ManipStart     = False
needUndoFirst ManipJustStart = error "this should never happen"
needUndoFirst _              = True

isManipulating :: ManipState -> Bool
isManipulating ManipJustStart = False
isManipulating ManipEnd       = False
isManipulating _              = True

data HandleWidgetConfig t = HandleWidgetConfig {
  _handleWidgetConfig_pfctx       :: PFWidgetCtx t
  -- | if this is Nothing, then the handle is effectively disabled
  -- the reason we need to disable handles is so that Manipulators can handle creating themselves (and thus no handles would exist yet)
  -- we do it inside of Handle to avoid switching to disable handles and just makes everything easier
  , _handleWidgetConfig_mbox      :: Behavior t (Maybe LBox) -- ^ if this is Nothing, the handle is effecitvely disabled
  , _handleWidgetConfig_graphic   :: Behavior t (Maybe Char)
  , _handleWidgetConfig_dragEv    :: Event t Drag2
  , _handleWidgetConfig_forceDrag :: Behavior t Bool
  , _handleWidgetConfig_cancel    :: Event t ()
}

data HandleWidget t = HandleWidget {
  _handleWidget_dragged           :: Event t (ManipState, (Int, Int))
  , _handleWidget_didCaptureInput :: Event t ()
}

-- TODO this needs to be able to render lines as well (or maybe that's a diff function)
holdHandle :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => HandleWidgetConfig t
  -> VtyWidget t m (HandleWidget t) -- ^ (manipulation state, drag to position)
holdHandle HandleWidgetConfig {..} = do
  -- draw image
  tellImages $ ffor
    (ffor3 _handleWidgetConfig_mbox _handleWidgetConfig_graphic (current . _pFWidgetCtx_attr_manipulator $ _handleWidgetConfig_pfctx) (,,))
    $ \(mbox,mgraphic,attr) -> case mbox of
      Nothing -> []
      Just (LBox (V2 x y) (V2 w h)) -> maybe [] (\graphic -> [V.translate x y $ V.charFill attr graphic w h]) mgraphic

  -- handle input
  let
    trackMouse ::
      Either () (Bool, Drag2)
      -> (ManipState, Maybe (Int, Int))
      -> PushM t (Maybe (ManipState, Maybe (Int, Int)))
    trackMouse (Left _) _ = return $ Just (ManipEnd, Nothing)
    trackMouse (Right (forceDrag, (Drag2 (fromX, fromY) (toX, toY) _ _ dstate))) (tracking, _) = do
      mbox <- sample _handleWidgetConfig_mbox
      return $ case mbox of
        Nothing -> Nothing
        Just lbox -> case dstate of
          -- TODO
          DragStart -> if does_lBox_contains_XY lbox (V2 fromX fromY)
            then Just (ManipJustStart,  Nothing)
            else Nothing
          Dragging | forceDrag || tracking == ManipJustStart ->
            Just (ManipStart, Just (toX-fromX, toY-fromY))
          Dragging -> if tracking /= ManipEnd
            then Just (Manipulating, Just (toX-fromX, toY-fromY))
            else Nothing
          DragEnd -> if tracking /= ManipEnd && tracking /= ManipJustStart
            then Just (ManipEnd, Just (toX-fromX, toY-fromY))
            else Nothing

  trackingDyn <- foldDynMaybeM trackMouse (ManipEnd, Nothing) $ leftmost
    [ fmap Left $ _handleWidgetConfig_cancel
    , fmap Right $ attach _handleWidgetConfig_forceDrag $ _handleWidgetConfig_dragEv]

  --debugStream [fmapLabelShow "track" $ ffilter (\x -> fst x /= ManipJustStart) $  updated trackingDyn]

  return
    HandleWidget {
      _handleWidget_dragged = fmapMaybe (\(ms, mp) -> mp >>= (\p -> return (ms, p))) $ updated trackingDyn
      , _handleWidget_didCaptureInput = updated trackingDyn $> ()
    }
