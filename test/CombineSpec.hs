module CombineSpec where

import TestsArbitrary
import FeaturesArbitrary
import Test.Hspec
import Test.QuickCheck
import Control.Lens
import Feature
import Tests
import Truth

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "combine" $ do
    it "should not modify list of features" $ do
      property (\(fs, ts) -> (combine fs ts) ^.. featureNames == fs ^.. featureNames)
    where
      featureNames = features.traverse.featureName
