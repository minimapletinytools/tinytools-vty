{-# OPTIONS_GHC -threaded #-}

module Main (
  main
) where

import           Prelude

import           Example
import           Flow
import           Potato



main :: IO ()
--main = exampleMain
--main = potatoMain
main = flowMain
