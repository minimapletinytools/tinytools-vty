{-# LANGUAGE UndecidableInstances #-}

module Potato.Flow.Reflex.Vty.PFWidget (
  PFWidget(..)
  , PFWidgetCtx
) where

import           Relude

import           Potato.Flow
import qualified Potato.Flow.Reflex.Vty.Attrs as PFA

import qualified Graphics.Vty                 as V
import           Reflex
import           Reflex.Host.Class            (MonadReflexCreateTrigger)
import           Reflex.Vty

import           Control.Monad.Fix

data PFWidgetCtx t = PFWidgetCtx {
  _pFWidgetCtx_attr_default       :: Dynamic t V.Attr
  , _pFWidgetCtx_attr_manipulator :: Dynamic t V.Attr
  , _pFWidgetCtx_ev_cancel        :: Event t ()
}

newtype PFWidget t m a = PFWidget { unPFWidget :: ReaderT (PFWidgetCtx t) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadSample t
    , MonadHold t
    , MonadFix
    , NotReady t
    , PostBuild t
    , TriggerEvent t
    , MonadReflexCreateTrigger t
    , MonadIO
    , HasDisplaySize t -- TODO make sure this actual works, don't I need DeriveAnyClass enabled?
    )

deriving instance PerformEvent t m => PerformEvent t (PFWidget t m)

instance MonadTrans (PFWidget t) where
  lift f = PFWidget $ lift f

instance MonadNodeId m => MonadNodeId (PFWidget t m) where
  getNextNodeId = PFWidget $ lift getNextNodeId

instance ImageWriter t m => ImageWriter t (PFWidget t m) where
  tellImages x = PFWidget . lift $ tellImages x

-- done via DeriveAnyClass I guess
--instance (Reflex t, Monad m) => HasDisplaySize t (PFWidget t m) where

instance (HasVtyInput t m, Monad m) => HasVtyInput t (PFWidget t m) where
  input = PFWidget . lift $ input

instance (HasFocus t m, Monad m) => HasFocus t (PFWidget t m) where
  focus = PFWidget . lift $ focus

-- | Runs a 'PFWidget' with a given context
runPFWidget
  :: (Reflex t, MonadNodeId m)
  => PFWidgetCtx t
  -> PFWidget t m a
  -> m a
runPFWidget ctx w = runReaderT (unPFWidget w) ctx
