{-# LANGUAGE UndecidableInstances #-}

module Potato.Flow.Reflex.Vty.PFWidget (
  PFWidgetCtx(..)
) where

import           Relude

import           Potato.Flow
import qualified Potato.Flow.Reflex.Vty.Attrs as PFA

import qualified Graphics.Vty                 as V
import           Reflex
import           Reflex.Vty


-- TODO rename
-- we don't use 'Reader' for this because it doesn't play well with 'Layout' taking a 'VtyWidget'
data PFWidgetCtx t = PFWidgetCtx {
  _pFWidgetCtx_attr_default       :: Dynamic t V.Attr
  , _pFWidgetCtx_attr_manipulator :: Dynamic t V.Attr
  , _pFWidgetCtx_ev_cancel        :: Event t ()
}
