{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell          #-}

module Potato.Flow.THSpec
  ( spec
  )
where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit   (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow.Vty.Main
import Potato.Flow
import Potato.Flow.ParamsSpec hiding (spec)

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Ref
import           Data.Default
import           Data.Kind
import qualified Data.List                  as L

import qualified Graphics.Vty               as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty
import           Reflex.Vty.Test.Monad.Host
import           Reflex.Vty.Test.Monad.Host.TH
import Reflex.Vty.Test.Common

import           Reflex.Vty.Test.Monad.Host.TH

import Language.Haskell.TH


spec :: Spec
spec = do
  return ()
  --fromHUnitTest $ TestLabel "TH TEST HALP" $ TestCase $
    --liftIO $ putStrLn $(stringE . show =<< reify ''ReflexVtyTestApp)
    --liftIO $ putStrLn $(stringE . show =<< reifyInstances ''ReflexVtyTestApp [AppT (AppT (ConT $ mkName "PotatoNetwork") (VarT $ mkName "t")) (VarT $ mkName "m")])
    --liftIO $ putStrLn $(stringE . show =<< reifyInstances ''ReflexVtyTestApp [VarT $ mkName "a"])
