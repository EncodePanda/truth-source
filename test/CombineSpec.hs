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

noTests :: Tests
noTests = Tests []

spec :: Spec
spec =
  describe "combine" $ do
    it "should not modify list of features (name)" $ do
      property (\(fs, ts) -> (combine fs ts) ^.. featureNames == fs ^.. featureNames)
    it "should not modify list of user stories (description) per feature" $ do
      property (\(fs, ts) -> (combine fs ts) ^.. userStoryNames == fs ^.. userStoryNames)
    it "should not modify list of criterna (name) per user story" $ do
      property (\(fs, ts) -> (combine fs ts) ^.. criteriaNames == fs ^.. criteriaNames)
    it "should mark all criterias' status as Missing if test list is empty" $ do
      property (\fs -> filter (== Missing) ((combine fs noTests) ^.. criteriaStatuses) == (replicate (length  $ fs ^.. criteriaStatuses) Missing))
    where
      featureNames = features.traverse.featureName
      userStoryNames = features.traverse.userStories.traverse.userStoryDesc
      criteriaNames = features.traverse.userStories.traverse.criteria.traverse.criteriaName
      criteriaStatuses = features.traverse.userStories.traverse.criteria.traverse.status
