{-# LANGUAGE TemplateHaskell #-}


module Potato.Flow.DefaultTermWidthFile where

import           Relude

import qualified Data.ByteString.Lazy                    as LBS
import           Data.FileEmbed                          (embedFile)


defaultTermWidthFileBS :: LBS.ByteString
defaultTermWidthFileBS = LBS.fromStrict $(embedFile "xterm-256color_termwidthfile")
