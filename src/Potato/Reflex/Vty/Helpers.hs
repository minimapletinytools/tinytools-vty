{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -threaded #-}

module Potato.Reflex.Vty.Helpers (
  debugFocus
  , debugInput
  , debugSize
  , dragTest
  , richTextConfig_simpleForeColorAttr
  , debugStream
  , countEv
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Testing
import           Reflex.Potato.Helpers


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Data.Functor.Misc
import qualified Data.Map                as Map
import           Data.Maybe
import qualified Data.Text               as T
import qualified Data.Text.Zipper        as TZ
import           Data.Time.Clock
import qualified Graphics.Vty            as V
import           Reflex
import           Reflex.Class.Switchable
import           Reflex.Network
import           Reflex.Vty

debugFocus :: (Reflex t, Monad m) => VtyWidget t m ()
debugFocus = do
  f <- focus
  text $ T.pack . show <$> current f

debugInput :: (Reflex t, MonadHold t m) => VtyWidget t m ()
debugInput = do
  lastEvent <- hold "No event yet" . fmap show =<< input
  text $ T.pack <$> lastEvent

debugSize ::  (Reflex t, MonadHold t m) => VtyWidget t m ()
debugSize = do
  ldw <- displayWidth
  ldh <- displayHeight
  let combine w h = "w: " <> show w <> " h: " <> show h
  text $ liftA2 combine (current ldw) (current ldh)

dragTest :: (Reflex t, MonadHold t m, MonadFix m) => VtyWidget t m ()
dragTest = do
  lastEvent <- hold "No event yet" . fmap show =<< drag V.BLeft
  text $ T.pack <$> lastEvent

richTextConfig_simpleForeColorAttr :: (Reflex t) => RichTextConfig t
richTextConfig_simpleForeColorAttr = RichTextConfig $ constant (V.defAttr { V.attrForeColor = V.SetTo V.yellow})

debugStream :: (Reflex t, MonadHold t m) => [Event t Text] -> VtyWidget t m ()
debugStream evs = do
  t <- holdDyn "" $ mergeWith (\a b -> a <> " | " <> b) evs
  richText richTextConfig_simpleForeColorAttr (current t)

countEv :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Dynamic t Int)
countEv ev = foldDyn (\_ b -> b+1) 0 ev
