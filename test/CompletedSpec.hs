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
