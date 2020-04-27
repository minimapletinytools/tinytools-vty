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

module Potato (
  potatoMain
) where
import           Relude

import           Potato.Flow


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Data.Functor.Misc
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Text               (Text)
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

dragTest :: (Reflex t, MonadHold t m, MonadFix m) => VtyWidget t m ()
dragTest = do
  lastEvent <- hold "No event yet" . fmap show =<< drag V.BLeft
  text $ T.pack <$> lastEvent

scrolling :: (Reflex t, MonadHold t m, MonadFix m, PostBuild t m, MonadNodeId m) => VtyWidget t m ()
scrolling = do
    f <- focus
    row $ do
      stretch $ col $ do
        fixed 2 $ text "Use your mouse wheel or up and down arrows to scroll:"
        out <- fixed 5 $ boxStatic def $ scrollableText never $ "Gallia est omnis divisa in partes tres, quarum unam incolunt Belgae, aliam Aquitani, tertiam qui ipsorum lingua Celtae, nostra Galli appellantur. Hi omnes lingua, institutis, legibus inter se differunt. Gallos ab Aquitanis Garumna flumen, a Belgis Matrona et Sequana dividit. Horum omnium fortissimi sunt Belgae, propterea quod a cultu atque humanitate provinciae longissime absunt, minimeque ad eos mercatores saepe commeant atque ea quae ad effeminandos animos pertinent important, proximique sunt Germanis, qui trans Rhenum incolunt, quibuscum continenter bellum gerunt. Qua de causa Helvetii quoque reliquos Gallos virtute praecedunt, quod fere cotidianis proeliis cum Germanis contendunt, cum aut suis finibus eos prohibent aut ipsi in eorum finibus bellum gerunt. Eorum una pars, quam Gallos obtinere dictum est, initium capit a flumine Rhodano, continetur Garumna flumine, Oceano, finibus Belgarum, attingit etiam ab Sequanis et Helvetiis flumen Rhenum, vergit ad septentriones. Belgae ab extremis Galliae finibus oriuntur, pertinent ad inferiorem partem fluminis Rheni, spectant in septentrionem et orientem solem. Aquitania a Garumna flumine ad Pyrenaeos montes et eam partem Oceani quae est ad Hispaniam pertinet; spectat inter occasum solis et septentriones.\nApud Helvetios longe nobilissimus fuit et ditissimus Orgetorix. Is M. Messala, [et P.] M. Pisone consulibus regni cupiditate inductus coniurationem nobilitatis fecit et civitati persuasit ut de finibus suis cum omnibus copiis exirent: perfacile esse, cum virtute omnibus praestarent, totius Galliae imperio potiri. Id hoc facilius iis persuasit, quod undique loci natura Helvetii continentur: una ex parte flumine Rheno latissimo atque altissimo, qui agrum Helvetium a Germanis dividit; altera ex parte monte Iura altissimo, qui est inter Sequanos et Helvetios; tertia lacu Lemanno et flumine Rhodano, qui provinciam nostram ab Helvetiis dividit. His rebus fiebat ut et minus late vagarentur et minus facile finitimis bellum inferre possent; qua ex parte homines bellandi cupidi magno dolore adficiebantur. Pro multitudine autem hominum et pro gloria belli atque fortitudinis angustos se fines habere arbitrabantur, qui in longitudinem milia passuum CCXL, in latitudinem CLXXX patebant."
        fixed 1 $ text $ ffor out $ \(ix, total) -> "Scrolled to line " <> T.pack (show ix) <> " of " <> T.pack (show total)
      fixed 20 $ text $ T.pack . show <$> current f


multilineTest :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => Event t (TZ.TextZipper -> TZ.TextZipper)
  -> VtyWidget t m (TextInput t)
multilineTest modifyEv = do
  let
    tic = TextInputConfig {
        _textInputConfig_initialValue = "meow meow meow"
        , _textInputConfig_modify = modifyEv
        , _textInputConfig_tabWidth = 2
        , _textInputConfig_display = constDyn id
      }
  multilineTextInput tic

fixedBox :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => VtyWidget t m ()
fixedBox = do
  dw <- displayWidth
  dh <- displayHeight
  let region1 = DynRegion (div' dw 6) (div' dh 6) (div' dw 2) (div' dh 2)
      region2 = DynRegion (div' dw 4) (div' dh 4) (2 * div' dw 3) (2 * div' dh 3)
  pane region1 (constDyn False) . boxStatic singleBoxStyle $ debugInput
  where
    div' :: (Integral a, Applicative f) => f a -> f a -> f a
    div' = liftA2 div

basicBox
  :: (Reflex t, MonadHold t m, PostBuild t m, MonadFix m, MonadNodeId m)
  => VtyWidget t m ()
basicBox = do
  let cfg = def { _textInputConfig_initialValue = "meow meow" }
  boxStatic roundedBoxStyle $ do
    f <- focus
    col $ do
      fixed 1 $ text $ T.pack . show <$> current f
      stretch $ multilineTextInput cfg
  return ()

potatoMain :: IO ()
potatoMain = mainWidget $ do
  currentTime <- liftIO $ getCurrentTime
  tickEv <- tickLossy 1 currentTime
  ticks <- foldDyn (+) 0 (fmap (const 1) tickEv)
  inp <- input
  col $ do
    fixed 1 $ text $ T.pack . show <$> current ticks
    stretch $ scrolling
    fixed 20 $ mdo
      t <- multilineTest (fmap (const (id)) tickEv)
      return ()
    stretch $ row $ do
      fixed 20 $ basicBox
      fixed 20 $ basicBox
      fixed 20 $ basicBox
    return $ fforMaybe inp $ \case
      V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
      _ -> Nothing
