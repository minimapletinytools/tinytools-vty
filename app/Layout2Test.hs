{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -threaded #-}

module Layout2Test (
  easyExample
) where
import           Relude

import           Potato.Flow


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Data.Functor.Misc
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Zipper                 as TZ
import qualified Graphics.Vty                     as V
import           Potato.Reflex.Vty.Widget.Layout
import           Reflex
import           Reflex.Class.Switchable
import           Reflex.Network
import           Reflex.Vty                       hiding (Constraint (..),
                                                   Orientation (..),
                                                   TileConfig (..), col, fixed,
                                                   row, runLayout, stretch,
                                                   tabNavigation, tile)


easyExample :: IO ()
easyExample = mainWidget $ do
  beginLayout $ row $ fixedL 39 $ col $ do
    (a1,b1,c1) <- fixedL 3 $ row $ do
      a <- stretch $ textButtonStatic def "POTATO"
      b <- stretch $ textButtonStatic def "TOMATO"
      c <- stretch $ textButtonStatic def "EGGPLANT"
      return (a,b,c)
    (a2,b2,c2) <- fixedL 3 $ row $ do
      a <- stretch $ textButtonStatic def "CHEESE"
      b <- stretch $ textButtonStatic def "BEES"
      c <- stretch $ textButtonStatic def "MY KNEES"
      return (a,b,c)
    (a3,b3,c3) <- fixedL 3 $ row $ do
      a <- stretch $ textButtonStatic def "TIME"
      b <- stretch $ textButtonStatic def "RHYME"
      c <- stretch $ textButtonStatic def "A BIG CRIME"
      return (a,b,c)
    -- NOTE the box will most likely not render correctly once you put emoji's
    -- you need to initialize vty with an updated char width map to fix this
    fixed 7 $ boxTitle (constant def) "CLICK BUTTONS TO DRAW" $ do
      outputDyn <- foldDyn (<>) "" $ mergeWith (<>) [a1 $> "🥔", b1 $> "🍅", c1 $> "🍆", a2 $> "🧀", b2 $> "🐝🐝", c2 $> "💘", a3 $> "⏰", b3 $> "📜", c3 $> "💰🔪🔒"]
      text (current outputDyn)
  inp <- input
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
