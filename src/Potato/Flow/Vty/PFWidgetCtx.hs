{-# LANGUAGE UndecidableInstances #-}

module Potato.Flow.Vty.PFWidgetCtx (
  PFWidgetCtx(..)
) where

import           Relude

import           Potato.Flow
import qualified Potato.Flow.Vty.Attrs as PFA

import qualified Graphics.Vty          as V
import           Reflex
import           Reflex.Vty


-- TODO rename
-- we don't use 'Reader' for this because it doesn't play well with 'Layout' taking a 'VtyWidget'
data PFWidgetCtx t = PFWidgetCtx {
  _pFWidgetCtx_attr_default              :: Dynamic t V.Attr
  , _pFWidgetCtx_attr_manipulator        :: Dynamic t V.Attr

  , _pFWidgetCtx_initialPFState          :: PFState

  , _pFWidgetCtx_goatWidget              :: GoatWidget t
  , _pFWidgetCtx_pFState                 :: Dynamic t PFState

}
