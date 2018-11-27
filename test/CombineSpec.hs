module CombineSpec where

import TestsArbitrary
import FeaturesArbitrary
import Test.Hspec
import Test.QuickCheck
import Control.Lens
import Feature
import Tests
import Truth

noTests :: Tests
noTests = Tests []

spec :: Spec
spec =
  describe "combine" $ do
    it "should not modify list of features (name)" $ do
      property (\(fs, ts) -> (combine fs ts) ^.. allNames == fs ^.. allNames)
    it "should not modify list of user stories (description) per feature" $ do
      property (\(fs, ts) -> (combine fs ts) ^.. allUserStories == fs ^.. allUserStories)
    it "should not modify list of criterna (name) per user story" $ do
      property (\(fs, ts) -> (combine fs ts) ^.. allCriteria.traverse.criteriaName == fs ^.. allCriteria.traverse.criteriaName)
    it "should mark all criterias' status as Missing if test list is empty" $ do
      property (\fs -> filter (/= Missing) ((combine fs noTests) ^.. allStatuses) == [])
    it "should mark criteria as Done if test passed" $ do
      let fs = featuresWithTest "test-name"
      let ts = Tests [Test "test-name" Passed]
      (combine fs ts)^..allStatuses `shouldBe` [Done]
    it "should mark criteria as NotDone if test not implemented" $ do
      let fs = featuresWithTest "test-name"
      let ts = Tests [Test "test-name" NotImplemented]
      (combine fs ts)^..allStatuses `shouldBe` [(NotDone NotImplemented)]
    it "should mark criteria as NotDone if test failed" $ do
      let fs = featuresWithTest "test-name"
      let ts = Tests [Test "test-name" Failed]
      (combine fs ts)^..allStatuses `shouldBe` [(NotDone Failed)]
    it "should mark criteria as NotDone if test failed" $ do
      let fs = featuresWithTest "test-name"
      let ts = Tests [Test "test-name" Regression]
      (combine fs ts)^..allStatuses `shouldBe` [(NotDone Regression)]
    it "should mark criteria as NotDone if unknown" $ do
      let fs = featuresWithTest "test-name"
      let ts = Tests [Test "test-name" Unknown]
      (combine fs ts)^..allStatuses `shouldBe` [(NotDone Unknown)]
    where
      allNames = features.traverse.featureName
      allUserStories = features.traverse.userStories.traverse.userStoryDesc
      allStatuses = features.traverse.userStories.traverse.criteria.traverse.status

featuresWithTest :: String -> Features
featuresWithTest testName = Features $ [Feature "n" [UserStory "us" [(criteria testName)]]]
  where
    criteria :: String -> Criteria
    criteria testName = Criteria "c" testName Missing []
