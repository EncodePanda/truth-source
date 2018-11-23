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
    it "should not modify list of features (name)" $ do
      property (\(fs, ts) -> (combine fs ts) ^.. featureNames == fs ^.. featureNames)
    it "should not modify list of user stories (description) per feature" $ do
      property (\(fs, ts) -> (combine fs ts) ^.. userStoryNames == fs ^.. userStoryNames)
    where
      featureNames = features.traverse.featureName
      userStoryNames = features.traverse.userStories.traverse.userStoryDesc
