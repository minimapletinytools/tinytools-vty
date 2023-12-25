{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

{-# OPTIONS_GHC -threaded #-}

module Potato.Reflex.Vty.Helpers (
  MonadWidget
  , MonadLayoutWidget
  , debugFocus
  , debugInput
  , debugSize
  , dragTest
  , richTextConfig_simpleForeColorAttr
  , debugStreamBeh
  , debugStream
  , fmapLabelShow
  , countEv
  , vLayoutPad
  , drag2AttachOnStart
) where


import           Relude

import           Potato.Reflex.Vty.Widget

import           Control.Monad.Fix
import           Control.Monad.NodeId
import qualified Data.Text                as T
import qualified Graphics.Vty             as V
import           Reflex
import           Reflex.Vty

type MonadWidget t m = (Reflex t, MonadHold t m, MonadFix m, NotReady t m, Adjustable t m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadNodeId m, MonadIO (Performable m), MonadSample t m, MonadIO m
      , HasImageWriter t m
      , MonadNodeId m
      , HasDisplayRegion t m
      , HasFocusReader t m
      , HasInput t m
      , HasTheme t m)

type MonadLayoutWidget t m = (MonadWidget t m, HasFocus t m, HasLayout t m)

debugFocus :: (HasFocusReader t m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => m ()
debugFocus = do
  f <- focus
  text $ T.pack . show <$> current f

debugInput :: (MonadHold t m, HasInput t m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => m ()
debugInput = do
  lastEvent <- hold "No event yet" . fmap show =<< input
  text $ T.pack <$> lastEvent

debugSize ::  (MonadHold t m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => m ()
debugSize = do
  ldw <- displayWidth
  ldh <- displayHeight
  let combine w h = "w: " <> show w <> " h: " <> show h
  text $ liftA2 combine (current ldw) (current ldh)

dragTest :: (MonadHold t m, MonadFix m, HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasTheme t m) => m ()
dragTest = do
  lastEvent <- hold "No event yet" . fmap show =<< drag V.BLeft
  text $ T.pack <$> lastEvent

richTextConfig_simpleForeColorAttr :: (Reflex t) => RichTextConfig t
richTextConfig_simpleForeColorAttr = RichTextConfig $ constant (V.defAttr { V.attrForeColor = V.SetTo V.yellow})

fmapLabelShow :: (Functor f, Show a) => Text -> f a -> f Text
fmapLabelShow t = fmap (\x -> t <> ": " <> show x)

-- TODO rename to debugStreamEv
debugStream :: (MonadHold t m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => [Event t Text] -> m ()
debugStream evs = do
  t <- holdDyn "" $ mergeWith (\a b -> a <> "\n" <> b) evs
  richText richTextConfig_simpleForeColorAttr (current t)

debugStreamBeh :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => [Behavior t Text] -> m ()
debugStreamBeh behs = text $ foldr (liftA2 (\t1 t2 -> t1 <> " " <> t2)) "" behs

countEv :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Dynamic t Int)
countEv ev = foldDyn (\_ b -> b+1) 0 ev

vLayoutPad :: (PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m, HasFocusReader t m, HasDisplayRegion t m, HasImageWriter t m, HasInput t m) => Int -> m a -> m a
vLayoutPad n w = initLayout $ col $ do
  (grout . fixed) (constDyn n) $ return ()
  (grout . stretch) 0 (lift w)

{-
dragAttachOnStart
  :: forall t m a. (Reflex t, MonadFix m, MonadHold t m)
  => V.Button
  -> Behavior t a
  -> m (Event t (a, Drag))
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


-- TODO DELETE UNUSED
drag2AttachOnStart
  :: forall t m a. (Reflex t, MonadFix m, MonadHold t m, HasInput t m)
  => V.Button
  -> Behavior t a
  -> m (Event t (a, Drag2))
drag2AttachOnStart btn beh = do
  -- TODO pretty sure this should be btn?
  dragEv <- drag2 V.BLeft
  let
    foldfn d ma = do
      anew <- case ma of
        Nothing                                   -> sample beh
        Just (_, _) | _drag2_state d == DragStart -> sample beh
        Just (a, _)                               -> return a
      return $ Just (anew, d)
  dragBeh <- foldDynM foldfn Nothing dragEv
  return $ fmapMaybe id $ updated dragBeh