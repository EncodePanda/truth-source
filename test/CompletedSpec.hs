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

    it "should mark empty user story as 'not defined'" $ do
      let us = UserStory "" []
      isCompleted us `shouldBe` NotDefined

    it "should mark user story as 'not defined' if all tests are missing" $ do
      isCompleted userStoryAllMissing `shouldBe` NotDefined

    it "should mark user story as 'partially defined' if contains at least one criterion with missing test" $ do
      isCompleted userStoryAtLeastOneMissing `shouldBe` PartiallyDefined

    it "should mark user story as 'in progress' with zero progress if all tests are not implemented" $ do
      isCompleted userStoryAllInProgress `shouldBe` (InProgress (0, 3))

    it "should mark user story as 'in progress' with some progress if all tests are either done or not implemented" $ do
      let us = UserStory "" [criterionDone,  criterionNotImplemented, criterionDone, criterionNotImplemented, criterionDone]
      isCompleted us `shouldBe` (InProgress (3, 5))

    it "should mark user story as 'failing' with at least one failing test but all at least defined" $ do
      isCompleted userStoryFailed `shouldBe` (Failing "")

    it "should mark user story as 'successful' if all tests are passing " $ do
      isCompleted userStoryDone `shouldBe` Successful

    it "should mark empty feature as 'not defined'" $ do
      let fs = Feature "" []
      isCompleted fs `shouldBe` NotDefined

    it "should mark feature as 'not defined' if all user stories are 'not defined'" $ do
      let fs = Feature "" [userStoryAllMissing, userStoryAllMissing]
      isCompleted fs `shouldBe` NotDefined

    it "should mark feature as 'partially defined' if contains at least one criterion with missing test" $ do
      let fs = Feature "" [userStoryAllMissing, userStoryDone, userStoryDone]
      isCompleted fs `shouldBe` PartiallyDefined

    it "should mark feature as 'in progress' with zero progress if all tests are not implemented" $ do
      let fs = Feature "" [userStoryAllInProgress, userStoryAllInProgress, userStoryAllInProgress]
      isCompleted fs `shouldBe` (InProgress (0, 3))

    it "should mark feature as 'in progress' with some progress if all tests are either done or not implemented" $ do
      let fs = Feature "" [userStoryDone, userStoryAllInProgress, userStoryDone, userStoryAllInProgress, userStoryDone]
      isCompleted fs `shouldBe` (InProgress (3, 5))

    it "should mark feature as 'failing' with at least one failing test but all at least defined" $ do
      let fs = Feature "" [userStoryDone, userStoryAllInProgress, userStoryFailed]
      isCompleted fs `shouldBe` (Failing "")

    it "should mark user story as 'successful' if all tests are passing " $ do
      let fs = Feature "" [userStoryDone, userStoryDone, userStoryDone, userStoryDone, userStoryDone]
      isCompleted fs `shouldBe` Successful

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

userStoryAllMissing :: UserStory
userStoryAllMissing = UserStory "" [criterionMissing, criterionMissing, criterionMissing]

userStoryAtLeastOneMissing :: UserStory
userStoryAtLeastOneMissing = UserStory "" [criterionDone, criterionNotImplemented, criterionMissing, criterionFailed, criterionRegression]

userStoryDone :: UserStory
userStoryDone = UserStory "" [criterionDone, criterionDone, criterionDone]

userStoryAllInProgress :: UserStory
userStoryAllInProgress = UserStory "" [criterionNotImplemented, criterionNotImplemented, criterionNotImplemented]

userStoryFailed :: UserStory
userStoryFailed =  UserStory "" [criterionDone, criterionNotImplemented, criterionFailed, criterionRegression]
