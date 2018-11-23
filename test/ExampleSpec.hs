module ExampleSpec where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "example description" $ do
  it "example it-should" $ do
    (1==1) `shouldBe` True
