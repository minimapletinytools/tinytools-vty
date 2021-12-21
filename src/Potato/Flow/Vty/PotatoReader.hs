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

import qualified System.FilePath as FP

import qualified Graphics.Vty as V
import Reflex.Host.Class (MonadReflexCreateTrigger)
import           Reflex
import           Reflex.Vty

import Potato.Flow.Vty.Attrs

data PotatoStyle = PotatoStyle {
  _potatoStyle_canvasCursor :: V.Attr
  , _potatoStyle_normal :: V.Attr
  , _potatoStyle_selected :: V.Attr
  , _potatoStyle_softSelected :: V.Attr
}


instance Default PotatoStyle where
  def = PotatoStyle {
      _potatoStyle_canvasCursor = lg_canvas_cursor
      , _potatoStyle_normal = lg_default
      , _potatoStyle_selected = lg_layer_selected
      , _potatoStyle_softSelected = lg_layer_inheritselect
    }

data PotatoConfig t = PotatoConfig {
  _potatoConfig_style :: Behavior t PotatoStyle

  -- TODO these need to be per document if you ever want MDI
  , _potatoConfig_appCurrentOpenFile :: Behavior t (Maybe FP.FilePath)
  , _potatoConfig_appCurrentDirectory :: Behavior t FP.FilePath
  , _potatoConfig_appPrintFile :: Behavior t (Maybe FP.FilePath)
  -- TODO
  --, _potatoConfig_unsavedChanges :: Behavior t Bool
}

instance (Reflex t) =>  Default (PotatoConfig t) where
  def = PotatoConfig {
      _potatoConfig_style = constant def
      , _potatoConfig_appCurrentOpenFile = constant Nothing
      , _potatoConfig_appPrintFile = constant Nothing
    }

-- | A class for things that can dynamically gain and lose focus
class (Reflex t, Monad m) => HasPotato t m | m -> t where
  askPotato :: m (PotatoConfig t)

instance (HasInput t m, Monad m) => HasInput t (ReaderT r m)


-- TODO it's better to do this using
-- default input :: (f m' ~ m, Monad m', MonadTrans f, HasInput t m') => ...
-- inside of HasFocus class
instance (Reflex t, HasFocus t m, Monad m) => HasFocus t (ReaderT r m) where
  makeFocus = lift makeFocus
  requestFocus = lift . requestFocus
  isFocused = lift . isFocused
  --subFoci :: m a -> m (a, Dynamic t FocusSet)
  subFoci x = ReaderT $ \r -> subFoci (runReaderT x r)
  focusedId = lift focusedId


instance HasPotato t m => HasPotato t (ReaderT x m)
instance HasPotato t m => HasPotato t (BehaviorWriterT t x m)
instance HasPotato t m => HasPotato t (DynamicWriterT t x m)
instance HasPotato t m => HasPotato t (EventWriterT t x m)
instance HasPotato t m => HasPotato t (NodeIdT m)
instance HasPotato t m => HasPotato t (Input t m)
instance HasPotato t m => HasPotato t (ImageWriter t m)
instance HasPotato t m => HasPotato t (DisplayRegion t m)
instance HasPotato t m => HasPotato t (FocusReader t m)
instance HasPotato t m => HasPotato t (Focus t m) where
  askPotato = lift askPotato
instance HasPotato t m => HasPotato t (Layout t m) where
  askPotato = lift askPotato


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

-- TODO it's better to do this using
-- default input :: (f m' ~ m, Monad m', MonadTrans f, HasInput t m') => ...
-- inside of HasLayout class
instance (Reflex t, HasLayout t m) => HasLayout t (PotatoReader t m) where
  axis a b c = PotatoReader . ReaderT $ \pcfg -> axis a b (runPotatoReader c pcfg)
  region = lift . region
  askOrientation = lift askOrientation

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (PotatoReader t m) where
  runWithReplace (PotatoReader a) e = PotatoReader $ runWithReplace a $ fmap unPotatoReader e
  traverseIntMapWithKeyWithAdjust f m e = PotatoReader $ traverseIntMapWithKeyWithAdjust (\k v -> unPotatoReader $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = PotatoReader $ traverseDMapWithKeyWithAdjust (\k v -> unPotatoReader $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = PotatoReader $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unPotatoReader $ f k v) m e

instance MonadTrans (PotatoReader t) where
  lift = PotatoReader . lift


instance MonadNodeId m => MonadNodeId (PotatoReader t m)


-- | Run a 'FocusReader' action with the given focus value
-- TODO flip arg order to match ReaderT oops...
runPotatoReader
  :: (Reflex t, Monad m)
  => PotatoReader t m a
  -> PotatoConfig t
  -> m a
runPotatoReader a b = flip runReaderT b $ unPotatoReader a
