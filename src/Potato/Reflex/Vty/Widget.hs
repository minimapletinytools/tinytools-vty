{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Potato.Reflex.Vty.Widget
  ( splitH
  , splitHDrag
  , DragState(..)
  , Drag2(..)
  , drag2
  , SingleClick(..)
  , singleClick
  , behaviorToggleWidget
  ) where

import           Prelude

import           Control.Applicative  (liftA2)
import           Graphics.Vty         (Image)
import qualified Graphics.Vty         as V

import           Reflex
import           Reflex.Class         ()
import           Reflex.Host.Class    (MonadReflexCreateTrigger)
import           Reflex.Vty.Host
import           Reflex.Vty.Widget


import           Control.Monad.NodeId
import           Control.Monad.Reader


-- new stuff

-- | A plain split of the available space into vertically stacked panes.
-- No visual separator is built in here.
splitH :: (Reflex t, Monad m, MonadNodeId m)
       => Dynamic t (Int -> Int)
       -- ^ Function used to determine size of first pane based on available size
       -> Dynamic t (Bool, Bool)
       -- ^ How to focus the two sub-panes, given that we are focused.
       -> VtyWidget t m a
       -- ^ Widget for first pane
       -> VtyWidget t m b
       -- ^ Widget for second pane
       -> VtyWidget t m (a,b)
splitH sizeFunD focD wA wB = do
  dw <- displayWidth
  dh <- displayHeight
  let regA = DynRegion
        { _dynRegion_left = pure 0
        , _dynRegion_top = pure 0
        , _dynRegion_width = sizeFunD <*> dw
        , _dynRegion_height = dh
        }
      regB = DynRegion
        { _dynRegion_left = _dynRegion_width regA
        , _dynRegion_top =  pure 0
        , _dynRegion_width = liftA2 (-) dw (_dynRegion_width regA)
        , _dynRegion_height = dh
        }
  ra <- pane regA (fst <$> focD) wA
  rb <- pane regB (snd <$> focD) wB
  return (ra,rb)


-- | A split of the available space into two parts with a draggable separator.
-- Starts with half the space allocated to each, and the first pane has focus.
-- Clicking in a pane switches focus.
splitHDrag :: (Reflex t, MonadFix m, MonadHold t m, MonadNodeId m)
  => Int -- ^ initial width of left panel
  -> VtyWidget t m ()
  -> VtyWidget t m a
  -> VtyWidget t m b
  -> VtyWidget t m (a,b)
splitHDrag splitter0 wS wA wB = mdo
  dh <- displayHeight
  dw <- displayWidth
  dragE <- drag V.BLeft
  splitterCheckpoint <- holdDyn splitter0 $ leftmost [fst <$> ffilter snd dragSplitter, resizeSplitter]
  splitterPos <- holdDyn splitter0 $ leftmost [fst <$> dragSplitter, resizeSplitter]
  splitterFrac <- holdDyn ((1::Double) / 2) $ ffor (attach (current dh) (fst <$> dragSplitter)) $ \(h, x) ->
    fromIntegral x / (max 1 (fromIntegral h))
  let dragSplitter = fforMaybe (attach (current splitterCheckpoint) dragE) $
        \(splitterX, Drag (fromX, _) (toX, _) _ _ end) ->
          if splitterX == fromX then Just (toX, end) else Nothing
      regA = DynRegion 0 0 splitterPos dh
      regS = DynRegion splitterPos 0 1 dh
      regB = DynRegion (splitterPos + 1) 0 (dw - splitterPos - 1) dh
      resizeSplitter = ffor (attach (current splitterFrac) (updated dw)) $
        \(frac, h) -> round (frac * fromIntegral h)
  focA <- holdDyn True $ leftmost
    [ True <$ mA
    , False <$ mB
    ]
  (mA, rA) <- pane2 regA focA $ withMouseDown wA
  pane regS (pure False) wS
  (mB, rB) <- pane2 regB (not <$> focA) $ withMouseDown wB
  return (rA, rB)
  where
    withMouseDown x = do
      m <- mouseDown V.BLeft
      x' <- x
      return (m, x')


-- updated stuff

data DragState = DragStart | Dragging | DragEnd deriving (Eq, Ord, Show)

-- | Same as 'Drag' but able to track drag start case
data Drag2 = Drag2
  { _drag2_from      :: (Int, Int) -- ^ Where the drag began
  , _drag2_to        :: (Int, Int) -- ^ Where the mouse currently is
  , _drag2_button    :: V.Button -- ^ Which mouse button is dragging
  , _drag2_modifiers :: [V.Modifier] -- ^ What modifiers are held
  , _drag2_state     :: DragState -- ^ Whether the drag ended (the mouse button was released)
  }
  deriving (Eq, Ord, Show)

-- | Same as 'drag' but returns 'Drag2' which tracks drag start events
drag2
  :: (Reflex t, MonadFix m, MonadHold t m)
  => V.Button
  -> VtyWidget t m (Event t Drag2)
drag2 btn = mdo
  inp <- input
  let f :: Maybe Drag2 -> V.Event -> Maybe Drag2
      f Nothing = \case
        V.EvMouseDown x y btn' mods
          | btn == btn' -> Just $ Drag2 (x,y) (x,y) btn' mods DragStart
          | otherwise   -> Nothing
        _ -> Nothing
      f (Just (Drag2 from _ _ mods st)) = \case
        V.EvMouseDown x y btn' mods'
          | st == DragEnd && btn == btn'  -> Just $ Drag2 (x,y) (x,y) btn' mods' DragStart
          | btn == btn'         -> Just $ Drag2 from (x,y) btn mods' Dragging
          | otherwise           -> Nothing -- Ignore other buttons.
        V.EvMouseUp x y (Just btn')
          | st == DragEnd        -> Nothing
          | btn == btn' -> Just $ Drag2 from (x,y) btn mods DragEnd
          | otherwise   -> Nothing
        V.EvMouseUp x y Nothing -- Terminal doesn't specify mouse up button,
                                -- assume it's the right one.
          | st == DragEnd      -> Nothing
          | otherwise -> Just $ Drag2 from (x,y) btn mods DragEnd
        _ -> Nothing
  let
    newDrag = attachWithMaybe f (current dragD) inp
  dragD <- holdDyn Nothing $ Just <$> newDrag
  return (fmapMaybe id $ updated dragD)

-- | Translates and crops an 'Image' so that it is contained by
-- the given 'Region'.
withinImage
  :: Region
  -> Image
  -> Image
withinImage (Region left top width height)
  | width < 0 || height < 0 = withinImage (Region left top 0 0)
  | otherwise = V.translate left top . V.crop width height


-- |
-- * 'Tracking' state means actively tracking the current stream of mouse events
-- * 'NotTracking' state means not tracking the current stream of mouse events
-- * 'WaitingForInput' means state will be set on next 'EvMouseDown' event
data MouseTrackingState = Tracking V.Button | NotTracking | WaitingForInput deriving (Show, Eq)

-- | same as pane except mouse drags that start off pane aren't reported and mouse drags that end off pane are reported
pane2
  :: forall t m a. (Reflex t, MonadHold t m, MonadNodeId m, MonadFix m)
  => DynRegion t
  -> Dynamic t Bool -- ^ Whether the widget should be focused when the parent is.
  -> VtyWidget t m a
  -> VtyWidget t m a
pane2 dr foc child = VtyWidget $ do
  ctx <- lift ask
  let
    reg = currentRegion dr
    isWithin :: Region -> Int -> Int -> Bool
    isWithin (Region l t w h) x y = not . or $ [ x < l
                                               , y < t
                                               , x >= l + w
                                               , y >= t + h ]
    trackMouse ::
      VtyEvent
      -> (MouseTrackingState, Maybe VtyEvent)
      -> PushM t (Maybe (MouseTrackingState, Maybe VtyEvent))
    trackMouse e (tracking, _) = do
      reg'@(Region l t _ _) <- sample reg
      -- consider using attachPromptlyDyn instead to get most up to date focus, which allows us to ignore mouse inputs when there is no focus (for stuff like ignoring mouse input when there is a popup)
      focused <- sample . current $ foc
      return $ case e of
        V.EvKey _ _ | not focused -> Nothing
        V.EvMouseDown x y btn m ->
          if tracking == Tracking btn || (tracking == WaitingForInput && isWithin reg' x y)
            then Just (Tracking btn, Just $ V.EvMouseDown (x - l) (y - t) btn m)
            else Just (NotTracking, Nothing)
        -- vty has mouse buttons override others (seems to be based on ordering of Button) when multiple are pressed.
        -- So it's possible for child widget to miss out on a 'EvMouseUp' event
        -- Perhaps a better option is to have both 'pane' and 'drag' report ALL mouse up events?
        V.EvMouseUp x y mbtn -> case mbtn of
          Nothing -> case tracking of
            Tracking _ -> Just (WaitingForInput, Just $ V.EvMouseUp (x - l) (y - t) mbtn)
            _ -> Just (WaitingForInput, Nothing)
          Just btn -> if tracking == Tracking btn
            -- only report EvMouseUp for the button we are tracking
            then Just (WaitingForInput, Just $ V.EvMouseUp (x - l) (y - t) mbtn)
            else Just (WaitingForInput, Nothing)
        _ -> Just (tracking, Just e)
  dynInputEvTracking <- foldDynMaybeM trackMouse (WaitingForInput, Nothing) $ _vtyWidgetCtx_input ctx
  let ctx' = VtyWidgetCtx
        { _vtyWidgetCtx_input = fmapMaybe snd $ updated dynInputEvTracking
        , _vtyWidgetCtx_focus = liftA2 (&&) (_vtyWidgetCtx_focus ctx) foc
        , _vtyWidgetCtx_width = _dynRegion_width dr
        , _vtyWidgetCtx_height = _dynRegion_height dr
        }
  (result, images) <- lift . lift $ runVtyWidget ctx' child
  let images' = liftA2 (\r is -> map (withinImage r) is) reg images
  tellImages images'
  return result

-- currently only works for a SINGLE POINT
-- TODO integrate with pane2 so it reports clicks that happen on pane.
data SingleClick = SingleClick
  { _singleClick_button      :: V.Button
  , _singleClick_coordinates :: (Int, Int) -- ^ coordinates of down click
  , _singleClick_modifiers   :: [V.Modifier]
  , _singleClick_didDragOff  :: Bool
  }
  deriving (Eq, Ord, Show)

singleClick :: (Reflex t, MonadHold t m, MonadFix m) => V.Button -> VtyWidget t m (Event t SingleClick)
singleClick btn = do
  let
    -- TODO implement for pane2 instead
    withinBounds (Drag2 (fromX, fromY) (toX, toY) _ _ _) = fromX == toX && fromY == toY
  dragEv <- drag2 btn
  didStayOnDyn <- foldDyn (const . withinBounds) False dragEv
  return $ flip push dragEv $ \d@(Drag2 (fromX, fromY) (toX, toY) _ mods _) -> do
    didStayOn <- sample . current $ didStayOnDyn
    return $ if withinBounds d
      then Just $ SingleClick btn (fromX, fromY) mods (not didStayOn)
      else Nothing


behaviorToggleWidget :: (Reflex t, MonadNodeId m) => Behavior t Bool -> VtyWidget t m a -> VtyWidget t m a -> VtyWidget t m (Behavior t a)
behaviorToggleWidget toggle wa wb = VtyWidget $ do
  ctx <- lift ask
  (a, bas) <- lift . lift $ runVtyWidget ctx wa
  (b, bbs) <- lift . lift $ runVtyWidget ctx wa
  tellImages $ ffor2 toggle bas (\t xs -> if t then xs else [])
  tellImages $ ffor2 toggle bbs (\t xs -> if not t then xs else [])
  return $ fmap (\t -> if t then a else b) toggle
