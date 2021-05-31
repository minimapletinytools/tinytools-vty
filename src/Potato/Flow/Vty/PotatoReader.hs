{-# Language UndecidableInstances #-}

module Potato.Flow.Vty.PotatoReader where

import           Relude

import Data.Default
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
--import Control.Monad.Morph
import Control.Monad.NodeId
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Ref
import Control.Monad.Trans (MonadTrans, lift)

import qualified Graphics.Vty as V
import Reflex.Host.Class (MonadReflexCreateTrigger)
import           Reflex
import           Reflex.Vty

import Potato.Flow.Vty.Attrs

data PotatoStyle t = PotatoStyle {
  _potatoStyle_canvasCursor :: Behavior t V.Attr
}

instance (Reflex t) => Default (PotatoStyle t) where
  def = PotatoStyle {
      _potatoStyle_canvasCursor = constant lg_canvas_cursor
    }

data PotatoConfig t = PotatoConfig {
  _potatoConfig_style :: PotatoStyle t
}

instance (Reflex t) =>  Default (PotatoConfig t) where
  def = PotatoConfig {
      _potatoConfig_style = def
    }

-- | A class for things that can dynamically gain and lose focus
class (Reflex t, Monad m) => HasPotato t m | m -> t where
  askPotato :: m (PotatoConfig t)

instance (HasInput t m, Monad m) => HasInput t (ReaderT x m)
instance (HasFocus t m, Monad m) => HasFocus t (ReaderT x m)

instance HasPotato t m => HasPotato t (ReaderT x m)
instance HasPotato t m => HasPotato t (BehaviorWriterT t x m)
instance HasPotato t m => HasPotato t (DynamicWriterT t x m)
instance HasPotato t m => HasPotato t (EventWriterT t x m)
instance HasPotato t m => HasPotato t (NodeIdT m)
instance HasPotato t m => HasPotato t (Input t m)
instance HasPotato t m => HasPotato t (ImageWriter t m)
instance HasPotato t m => HasPotato t (DisplayRegion t m)
instance HasPotato t m => HasPotato t (FocusReader t m)

-- | A widget that has access to information about whether it is focused
newtype PotatoReader t m a = PotatoReader
  { unPotatoReader :: ReaderT (PotatoConfig t) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadRef
    , MonadSample t
    )

instance (Monad m, Reflex t) => HasPotato t (PotatoReader t m) where
  askPotato = PotatoReader ask

deriving instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (PotatoReader t m)
deriving instance NotReady t m => NotReady t (PotatoReader t m)
deriving instance PerformEvent t m => PerformEvent t (PotatoReader t m)
deriving instance PostBuild t m => PostBuild t (PotatoReader t m)
deriving instance TriggerEvent t m => TriggerEvent t (PotatoReader t m)
deriving instance (HasInput t m, Monad m) => HasInput t (PotatoReader t m)
deriving instance HasFocus t m => HasFocus t (PotatoReader t m)
deriving instance HasFocusReader t m => HasFocusReader t (PotatoReader t m)
deriving instance HasTheme t m => HasTheme t (PotatoReader t m)
deriving instance HasDisplayRegion t m => HasDisplayRegion t (PotatoReader t m)

-- can't seem to include Control.Monad.Morph :(
--instance HasImageWriter t m => HasImageWriter t (PotatoReader t m)
--instance MFunctor (PotatoReader t) where
--  hoist f = PotatoReader . hoist f . unPotatoReader

instance HasImageWriter t m => HasImageWriter t (PotatoReader t m) where
  tellImages = lift . tellImages
  mapImages f = hoistpotato (mapImages f) where
    hoistpotato g = PotatoReader . (hoist g) . unPotatoReader
    hoist nat m = ReaderT (\i -> nat (runReaderT m i))

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (PotatoReader t m) where
  runWithReplace (PotatoReader a) e = PotatoReader $ runWithReplace a $ fmap unPotatoReader e
  traverseIntMapWithKeyWithAdjust f m e = PotatoReader $ traverseIntMapWithKeyWithAdjust (\k v -> unPotatoReader $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = PotatoReader $ traverseDMapWithKeyWithAdjust (\k v -> unPotatoReader $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = PotatoReader $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unPotatoReader $ f k v) m e

instance MonadTrans (PotatoReader t) where
  lift = PotatoReader . lift


instance MonadNodeId m => MonadNodeId (PotatoReader t m)

-- | Run a 'FocusReader' action with the given focus value
runPotatoReader
  :: (Reflex t, Monad m)
  => PotatoConfig t
  -> PotatoReader t m a
  -> m a
runPotatoReader b = flip runReaderT b . unPotatoReader
