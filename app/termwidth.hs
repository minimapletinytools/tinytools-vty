{-# OPTIONS_GHC -threaded #-}

import           Relude

import           Data.Maybe

import qualified Graphics.Vty                         as V
import qualified Graphics.Vty.UnicodeWidthTable.IO    as V
import qualified Graphics.Vty.UnicodeWidthTable.Query as V



main :: IO ()
main = do
  mTermName <- V.currentTerminalName
  let
    widthMapFile = fromJust mTermName <> "_termwidthfile"
  putStrLn $ "writing width for term: " <> show mTermName
  table <- V.buildUnicodeWidthTable '🥱'
  V.writeUnicodeWidthTable widthMapFile table
