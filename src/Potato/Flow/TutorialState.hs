{-# LANGUAGE TemplateHaskell #-}


module Potato.Flow.TutorialState (tutorialState) where

import           Relude

import           Potato.Flow
import qualified Potato.Flow.Serialization.SnakeWrangler as Snake

import           Data.ByteString
import qualified Data.ByteString.Lazy as LBS
import           Data.FileEmbed  (embedFile)

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import           Potato.Flow.TestStates



tutorialState :: (OwlPFState, ControllerMeta)
tutorialState = fromMaybe (owlpfstate_newProject, emptyControllerMeta)  . fmap (\(x, cm) -> (sPotatoFlow_to_owlPFState x, cm)) $ case Snake.deserialize (LBS.fromStrict tutorialjson) of
  --Left e -> trace e Nothing
  Left e -> Nothing
  Right j -> Just j

tutorialjson :: ByteString
tutorialjson = $(embedFile "tutorial.potato")
