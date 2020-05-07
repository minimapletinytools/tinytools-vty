module Potato.Flow.Reflex.Vty.Manipulator.Types (
  ManipState(..)
  , ManipSelectionType(..)
  , ManipOutput
  , ManipWidget
) where

import           Relude

import           Potato.Flow
import           Reflex
import           Reflex.Vty

data ManipState = ManipJustStart | ManipStart | Manipulating | ManipEnd deriving (Show, Eq)
data ManipSelectionType = MSTNone | MSTBox | MSTLine | MSTText | MSTBBox deriving (Show, Eq)
type ManipOutput t = (Event t (ManipState, Either ControllersWithId (LayerPos, SEltLabel)), Event t ())
type ManipWidget t m = VtyWidget t m (ManipOutput t)
