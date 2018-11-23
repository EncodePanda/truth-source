module CombineSpec where

import TestsArbitrary
import FeaturesArbitrary
import Test.Hspec
import Test.QuickCheck

import Feature
import Tests
import Truth

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "combine" $ do
    it "should not modify list of features" $ do
      property (\(fs, ts) -> featuresName (combine fs ts) == featuresName fs)
    where
      featuresName :: Features -> [String]
      featuresName (Features fs) = fmap _name fs
