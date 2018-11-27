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
    it "should mark empty feature as 'not defined' and `danger`" $ do
      let fs = Feature "" []
      label fs `shouldBe` "not defined"
      klazz fs `shouldBe` "danger"
