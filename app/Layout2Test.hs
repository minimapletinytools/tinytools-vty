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

module Layout2Test (
  layoutTestMain
  , easyExample
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
import qualified Graphics.Vty            as V
import           Reflex
import           Reflex.Class.Switchable
import           Reflex.Network
import           Reflex.Vty hiding (row, col, fixed, stretch, tile, Orientation (..), Constraint (..))
import Potato.Reflex.Vty.Widget.Layout2


easyExample :: IO ()
easyExample = mainWidget $ do
  beginLayoutD $ col $ do
    (a1,b1,c1) <- fixedD 3 $ row $ do
      a <- fixed 15 $ textButtonStatic def "POTATO"
      b <- fixed 15 $ textButtonStatic def "TOMATO"
      --c <- stretch $ textButtonStatic def "EGGPLANT"
      c <- stretchD $ row $ do
        stretch $ textButtonStatic def "A"
        stretch $ textButtonStatic def "B"
        stretch $ textButtonStatic def "C"
      return (a,b,c)
    (a2,b2,c2) <- fixedD 3 $ row $ do
      a <- stretch $ textButtonStatic def "CHEESE"
      b <- stretch $ textButtonStatic def "BEES"
      c <- stretch $ textButtonStatic def "ARROW IN MY KNEE"
      stretch $ textButtonStatic def "boop"
      stretch $ textButtonStatic def "doop"
      stretch $ textButtonStatic def "goop"
      return (a,b,c)
    (a3,b3,c3) <- fixedD 3 $ row $ do
      a <- stretch $ textButtonStatic def "TIME"
      b <- stretch $ textButtonStatic def "RHYME"
      c <- stretch $ textButtonStatic def "A BIG CRIME"
      return (a,b,c)
    fixedD 3 $ dummy
    fixedD 3 $ dummy
    fixedD 3 $ dummy
    fixedD 3 $ dummy
    return ()
  inp <- input
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

data Example = Example_TextEditor
             | Example_Todo
             | Example_ScrollableTextDisplay
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

layoutTestMain :: IO ()
layoutTestMain = mainWidget $ do
  inp <- input
  let buttons = beginLayout $ col $ do
        fixed 4 $ col $ do
          fixed 1 $ text "Select an example."
          fixed 1 $ text "Esc will bring you back here."
          fixed 1 $ text "Ctrl+c to quit."
        a <- fixed 5 $ textButtonStatic def "Todo List"
        b <- fixed 5 $ textButtonStatic def "Text Editor"
        c <- fixed 5 $ textButtonStatic def "Scrollable text display"
        return $ leftmost
          [ Left Example_Todo <$ a
          , Left Example_TextEditor <$ b
          , Left Example_ScrollableTextDisplay <$ c
          ]
      escapable w = do
        void w
        i <- input
        return $ fforMaybe i $ \case
          V.EvKey V.KEsc [] -> Just $ Right ()
          _ -> Nothing
  rec out <- networkHold buttons $ ffor (switch (current out)) $ \case
        Left Example_TextEditor -> escapable testBoxes
        Left Example_Todo -> escapable (return ())
        Left Example_ScrollableTextDisplay -> escapable scrolling
        Right () -> buttons
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

testBoxes
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => VtyWidget t m ()
testBoxes = do
  dw <- displayWidth
  dh <- displayHeight
  let region1 = DynRegion (div' dw 6) (div' dh 6) (div' dw 2) (div' dh 2)
      region2 = DynRegion (div' dw 4) (div' dh 4) (2 * div' dw 3) (2 * div' dh 3)
  pane region1 (constDyn False) . boxStatic singleBoxStyle $ debugInput
  _ <- pane region2 (constDyn True) . boxStatic singleBoxStyle $
    let cfg = def
          { _textInputConfig_initialValue =
            "This box is a text input. The box below responds to mouse drag inputs. You can also drag the separator between the boxes to resize them."
          }
        textBox = boxStatic roundedBoxStyle $ multilineTextInput cfg
        dragBox = boxStatic roundedBoxStyle dragTest
    in splitVDrag (hRule doubleBoxStyle) textBox dragBox
  return ()
  where
    div' :: (Integral a, Applicative f) => f a -> f a -> f a
    div' = liftA2 div

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

testStringBox :: (Reflex t, Monad m, MonadNodeId m) => VtyWidget t m ()
testStringBox = boxStatic singleBoxStyle .
  text . pure . T.pack . take 500 $ cycle ('\n' : ['a'..'z'])

scrolling :: forall t m. (Reflex t, MonadHold t m, MonadFix m, PostBuild t m, MonadNodeId m) => VtyWidget t m ()
scrolling = beginLayout $ col $ do
  fixed 2 $ text "Use your mouse wheel or up and down arrows to scroll:"
  out :: Behavior t (Int, Int) <- fixed 5 $ boxStatic def $ scrollableText never $ "Gabant."
  fixed 1 $ text $ ffor out $ \(ix, total) -> "Scrolled to line " <> T.pack (show ix) <> " of " <> T.pack (show total)
