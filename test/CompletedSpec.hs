module CompletedSpec where

import TestsArbitrary
import FeaturesArbitrary
import Test.Hspec
import Control.Lens
import Feature
import Tests
import Truth
import Presentation

spec :: Spec
spec =
  describe "completed" $ do
    it "should mark empty feature as 'not defined'" $ do
      let fs = Feature "" []
      isCompleted fs `shouldBe` NotDefined

    it "should mark empty user story as 'not defined'" $ do
      let us = UserStory "" []
      isCompleted us `shouldBe` NotDefined

    it "should mark user story as 'not defined' if all tests are missing" $ do
      let us = UserStory "" [criterionMissing, criterionMissing, criterionMissing]
      isCompleted us `shouldBe` NotDefined

    it "should mark user story as 'partially defined' if contains at least one criterion with missing test" $ do
      let us = UserStory "" [criterionDone, criterionNotImplemented, criterionMissing, criterionFailed, criterionRegression]
      isCompleted us `shouldBe` PartiallyDefined

    it "should mark user story as 'in progress' with zero progress if all tests are not implemented" $ do
      let us = UserStory "" [criterionNotImplemented, criterionNotImplemented, criterionNotImplemented]
      isCompleted us `shouldBe` (InProgress (0, 3))

    it "should mark user story as 'in progress' with some progress if all tests are either done or not implemented" $ do
      let us = UserStory "" [criterionDone,  criterionNotImplemented, criterionDone, criterionNotImplemented, criterionDone]
      isCompleted us `shouldBe` (InProgress (3, 5))

    it "should mark user story as 'failing' with at least one failing test but all at least defined" $ do
      let us = UserStory "" [criterionDone, criterionNotImplemented, criterionFailed, criterionRegression]
      isCompleted us `shouldBe` (Failing "")




criterionMissing :: Criteria
criterionMissing = Criteria "_" "_" Missing []

criterionDone :: Criteria
criterionDone = Criteria "_" "_" Done []

criterionNotImplemented :: Criteria
criterionNotImplemented = Criteria "_" "_" (NotDone NotImplemented) []

criterionFailed :: Criteria
criterionFailed = Criteria "_" "_" (NotDone Failed) []

criterionRegression :: Criteria
criterionRegression = Criteria "_" "_" (NotDone Regression) []
