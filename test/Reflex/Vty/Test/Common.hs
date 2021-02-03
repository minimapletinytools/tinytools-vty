module Reflex.Vty.Test.Common
  ( subscribeDynamic
  , readDynamic
  , checkSingle
  , checkSingleMaybe
  )
where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit   (fromHUnitTest)
import           Test.HUnit

import           Control.Monad.IO.Class     (liftIO)
import           Data.Kind
import qualified Data.List as L

import qualified Graphics.Vty               as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty
import           Reflex.Vty.Test.Monad.Host


{-
subscribeDynamic :: (TestGuestConstraints t m) => Dynamic t a -> m (EventHandle t a, Behavior t a)
subscribeDynamic d = do
  eh <- subscribeEvent $ updated d
  return (eh, current d)

readDynamic :: (TestGuestConstraints t m) => (EventHandle t a, Behavior t a) -> ReadPhase m a
readDynamic (evh, b) = do
  v <- sequence =<< readEvent evh
  case v of
    Nothing -> sample b
    Just x -> return x
-}

subscribeDynamic = undefined
readDynamic = undefined


checkSingle :: (HasCallStack, Eq a, Show a) => [a] -> a -> Assertion
checkSingle values a = case nonEmpty values of
  Nothing -> assertFailure "empty list"
  Just x  -> a @=? head x

checkSingleMaybe :: (HasCallStack, Eq a, Show a) => [Maybe a] -> a -> Assertion
checkSingleMaybe values a = case nonEmpty values of
  Nothing -> assertFailure "empty list"
  Just x  -> Just a @=? head x
