{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Potato.Reflex.Vty.Widget.Input
  ( 
  ) where

import Prelude

import Control.Applicative (liftA2)
import Control.Monad.Fix (MonadFix)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import Graphics.Vty (Image)
import qualified Graphics.Vty as V

import Reflex
import Reflex.Class ()
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Reflex.Vty.Widget
import Reflex.Vty.Host


import Control.Monad.NodeId
