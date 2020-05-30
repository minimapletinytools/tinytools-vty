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
  ( SingleClick(..)
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
  return $ flip push dragEv $ \d@(Drag2 (fromX, fromY) (toX, toY) _ mods ds) -> do
    didStayOn <- sample . current $ didStayOnDyn
    return $ if ds == DragEnd && withinBounds d
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
