module PotatoSpec
  ( spec
  )
where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit       ( fromHUnitTest )
import           Test.HUnit


some_io_test :: Test
some_io_test = TestLabel "testtest" $ TestCase $ return ()



some_hspec_test :: Spec
some_hspec_test = do
  it "dummy test" $ True `shouldBe` True



spec :: Spec
spec = do
  describe "Potato" $ do
    fromHUnitTest some_io_test
    some_hspec_test
