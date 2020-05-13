{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.CanvasPane (
  CursorState(..)
  , cursorDragStateEv
) where

import           Relude

import           Potato.Flow
import           Potato.Reflex.Vty.Widget

import qualified Text.Show

import           Reflex
import           Reflex.Vty




data CursorState = CSPan | CSSelecting | CSBox deriving (Eq)

instance Show CursorState where
  show CSPan       = "PAN"
  show CSSelecting = "SELECT"
  show CSBox       = "BOX"


-- TODO rename to canvasCursorDragStateEv
-- TODO probably can delete panPos from the tuple, it's only needed by pan
-- returns pan position at start of drag and dragging info filtered for cursor/drag state
cursorDragStateEv :: (Reflex t)
  => Maybe CursorState -- ^ cursor state to select for
  -> Maybe DragState
  -> Event t ((CursorState, (Int,Int)), Drag2) -- ^ ((cursor, panPos), drag)
  -> Event t ((Int,Int), Drag2)
cursorDragStateEv c' d' dragEv = r where
  fmapMaybeFn ((c,p),d) = if maybe True (_drag2_state d  ==) d' && maybe True (c ==) c'
    then Just (p,d)
    else Nothing
  r = fmapMaybe fmapMaybeFn dragEv
