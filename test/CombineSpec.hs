module CombineSpec where

import Test.Hspec
import Test.QuickCheck

import Truth
import Feature
import Tests

instance Arbitrary Features where
  arbitrary = do
    features <- listOf arbitrary
    return $ Features features

instance Arbitrary Feature where
  arbitrary = do
    name <- arbitrary
    userStories <- listOf arbitrary
    return $ Feature name userStories

instance Arbitrary UserStory where
  arbitrary = do
    desc <- arbitrary
    return $ UserStory desc []

instance Arbitrary Criteria where
  arbitrary = do
    name <- arbitrary
    status <- arbitrary
    steps <- listOf arbitrary
    return $ Criteria name status steps

instance Arbitrary Step where
  arbitrary = fmap Step arbitrary

instance Arbitrary Status where
  arbitrary = elements [Done, NotDone]



main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "combine" $ do
  it "should not modify list of features" $ do
    property (\fs -> combine fs (Tests []) == fs)

