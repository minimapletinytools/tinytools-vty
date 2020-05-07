{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Manipulator.Box (
  BoxHandleType(..)
) where

import           Relude


import           Potato.Flow
import           Potato.Flow.Reflex.Vty.CanvasPane
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget

import           Control.Exception
import           Control.Lens                       (over, _1)
import           Control.Monad.Fix
import           Data.Dependent.Sum                 (DSum ((:=>)))
import qualified Data.IntMap.Strict                 as IM
import qualified Data.List.NonEmpty                 as NE
import           Data.These
import           Data.Tuple.Extra

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

data BoxHandleType = BH_TL | BH_TR | BH_BL | BH_BR | BH_T | BH_B | BH_L | BH_R deriving (Show, Eq, Enum)

--let brBeh = ffor2 _manipulatorWidgetConfig_panPos (current boxManip_dlboxDyn) (makeCornerHandlePos bht)

makeCornerHandlePos ::
  BoxHandleType
  -> (Int, Int) -- ^ canvas pan position
  -> LBox -- ^ box being manipulated
  -> (Int, Int)
makeCornerHandlePos bht (px, py) (LBox (V2 x y) (V2 w h)) = case bht of
  BH_BR -> (r, b)
  BH_TL -> (l, t)
  BH_TR -> (r, t)
  BH_BL -> (b, l)
  _     -> error "don't use this for non-corner handles"
  where
    l = x+px
    t = y+py
    r = x+px+w
    b = y+py+h


--Just $ (,) ms $ Left $ IM.singleton _mBox_target $ CTagBox :=> (Identity $ CBox {
--  _cBox_deltaBox = makeDeltaBox bht (dx, dy)
--})


makeDeltaBox :: BoxHandleType -> (Int, Int) -> DeltaLBox
makeDeltaBox bht (dx,dy) = case bht of
  BH_BR -> DeltaLBox 0 $ V2 dx dy
  BH_TL -> DeltaLBox (V2 dx dy) 0
  BH_TR -> DeltaLBox (V2 0 dy) (V2 dx 0)
  BH_BL -> DeltaLBox (V2 dx 0) (V2 0 dy)
  BH_T  -> DeltaLBox (V2 0 dy) 0
  BH_B  -> DeltaLBox 0 (V2 0 dy)
  BH_L  -> DeltaLBox (V2 dx 0) 0
  BH_R  -> DeltaLBox 0 (V2 dx 0)
