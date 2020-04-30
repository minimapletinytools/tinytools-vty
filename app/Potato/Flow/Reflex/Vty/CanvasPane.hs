{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.CanvasPane (
  CursorState(..)
  , cursorDragStateEv
  , dynLBox_to_dynRegion
  , translate_dynRegion
) where

import           Relude

import           Potato.Flow
import           Potato.Reflex.Vty.Widget
import           Reflex.Potato.Helpers

import           Control.Monad.Fix
import           Data.Dependent.Sum       (DSum ((:=>)))
import qualified Data.IntMap.Strict       as IM
import           Data.These
import qualified Text.Show

import           Reflex
import           Reflex.Vty




data CursorState = CSPan | CSSelecting | CSBox deriving (Eq)

instance Show CursorState where
  show CSPan       = "PAN"
  show CSSelecting = "SELECT"
  show CSBox       = "BOX"

-- TODO rename to canvasCursorDragStateEv
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


dynLBox_to_dynRegion :: (Reflex t) => Dynamic t LBox -> DynRegion t
dynLBox_to_dynRegion dlb = r where
  x' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> x
  y' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> y
  w' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> w
  h' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> h
  r = DynRegion x' y' w' h'

translate_dynRegion :: (Reflex t) => Dynamic t (Int, Int) -> DynRegion t -> DynRegion t
translate_dynRegion pos dr = dr {
    _dynRegion_left = liftA2 (+) (_dynRegion_left dr) (fmap fst pos)
    , _dynRegion_top = liftA2 (+) (_dynRegion_top dr) (fmap snd pos)
  }
