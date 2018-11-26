module ToDocSpec where

import TestsArbitrary
import FeaturesArbitrary
import Test.Hspec
import Test.QuickCheck
import Text.Pandoc
import Control.Lens
import Feature
import Tests
import Truth

spec :: Spec
spec =
  describe "summary" $ do
    it "should return an empty Pandoc when features list is empty" $ do
      summary (Features $ []) `shouldBe` Pandoc nullMeta [Null]
    it "should put features in a table" $ do
      forAll (nonEmptyLimitedListOf (arbitrary)) (\(fs) -> (summary (Features fs)) `shouldBe` Pandoc nullMeta [
                     Table [Str "Features"] [AlignLeft] [0] [] []
                   ])
