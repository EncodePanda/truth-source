module FeaturesArbitrary where

import Test.QuickCheck
import Feature

limitedListOf :: Gen a -> Gen [a]
limitedListOf gen = do
  max <- elements [5..10]
  list <- listOf gen
  return $ take max list

instance Arbitrary Features where
  arbitrary = do
    features <- limitedListOf arbitrary
    return $ Features features

instance Arbitrary Feature where
  arbitrary = do
    name <- arbitrary
    userStories <- limitedListOf arbitrary
    return $ Feature name userStories

instance Arbitrary UserStory where
  arbitrary = do
    desc <- arbitrary
    criterias <- limitedListOf arbitrary
    return $ UserStory desc criterias

instance Arbitrary Criteria where
  arbitrary = do
    name <- arbitrary
    testName <- arbitrary
    status <- arbitrary
    steps <- limitedListOf arbitrary
    return $ Criteria name testName status steps

instance Arbitrary Step where
  arbitrary = fmap Step arbitrary

instance Arbitrary Status where
  arbitrary = elements [Missing]
