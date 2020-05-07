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
  MonadWidget
  , debugFocus
  , debugInput
  , debugSize
  , dragTest
  , richTextConfig_simpleForeColorAttr
  , debugStream
  , fmapLabelShow
  , countEv
  , vLayoutPad
  , drag2AttachOnStart
) where


import           Relude

import           Potato.Flow
import           Potato.Flow.Testing
import           Potato.Reflex.Vty.Widget
import           Reflex.Potato.Helpers


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Data.Functor.Misc
import qualified Data.Map                 as Map
import           Data.Maybe
import qualified Data.Text                as T
import qualified Data.Text.Zipper         as TZ
import           Data.Time.Clock
import qualified Graphics.Vty             as V
import           Reflex
import           Reflex.Class.Switchable
import           Reflex.Network
import           Reflex.Vty

type MonadWidget t m = (Reflex t, MonadHold t m, MonadFix m, NotReady t m, Adjustable t m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadNodeId m, MonadIO (Performable m), MonadIO m)

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

fmapLabelShow :: (Reflex t, Show a) => Text -> Event t a -> Event t Text
fmapLabelShow t = fmap (\x -> t <> ": " <> show x)

debugStream :: (Reflex t, MonadHold t m) => [Event t Text] -> VtyWidget t m ()
debugStream evs = do
  t <- holdDyn "" $ mergeWith (\a b -> a <> "\n" <> b) evs
  richText richTextConfig_simpleForeColorAttr (current t)

countEv :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Dynamic t Int)
countEv ev = foldDyn (\_ b -> b+1) 0 ev

vLayoutPad :: (Reflex t, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m, Monad m) => Int -> VtyWidget t m a -> VtyWidget t m a
vLayoutPad n w = col $ do
  fixed 5 $ return ()
  stretch w

{-
dragAttachOnStart
  :: forall t m a. (Reflex t, MonadFix m, MonadHold t m)
  => V.Button
  -> Behavior t a
  -> VtyWidget t m (Event t (a, Drag))
dragAttachOnStart btn beh = mdo
  inp <- input
  let
    f :: (Maybe (a, Drag), V.Event) -> PushM t (Maybe (a, Drag))
    f (Nothing, inp) = case inp of
      V.EvMouseDown x y btn' mods
        | btn == btn' -> do
          a <- sample beh
          return . Just $ (a, Drag (x,y) (x,y) btn' mods False)
        | otherwise -> return Nothing
      _ -> return Nothing
    f (Just (a, Drag from _ _ mods end), inp) = case inp of
      V.EvMouseDown x y btn' mods'
        | end && btn == btn' -> do
          newa <- sample beh
          return . Just $ (newa, Drag (x,y) (x,y) btn' mods' False)
        | btn == btn' -> return . Just $ (a, Drag from (x,y) btn mods' False)
        | otherwise   -> return Nothing -- Ignore other buttons.
      V.EvMouseUp x y (Just btn')
        | end         -> return Nothing
        | btn == btn' -> return . Just $ (a, Drag from (x,y) btn mods True)
        | otherwise   -> return Nothing
      V.EvMouseUp x y Nothing -- Terminal doesn't specify mouse up button,
                              -- assume it's the right one.
        | end       -> return Nothing
        | otherwise -> return . Just $ (a, Drag from (x,y) btn mods True)
      _ -> return Nothing
    newDrag :: Event t (a, Drag)
    newDrag = push f (attach (current dragD) inp)
  dragD <- holdDyn Nothing $ Just <$> newDrag
  return (fmapMaybe id $ updated dragD)
-}


drag2AttachOnStart
  :: forall t m a. (Reflex t, MonadFix m, MonadHold t m)
  => V.Button
  -> Behavior t a
  -> VtyWidget t m (Event t (a, Drag2))
drag2AttachOnStart btn beh = do
  dragEv <- drag2 V.BLeft
  let
    foldfn d ma = do
      anew <- case ma of
        Nothing                                   -> sample beh
        Just (a, _) | _drag2_state d == DragStart -> sample beh
        Just (a, _)                               -> return a
      return $ Just (anew, d)
  dragBeh <- foldDynM foldfn Nothing dragEv
  return $ fmapMaybe id $ updated dragBeh
