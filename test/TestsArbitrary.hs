module TestsArbitrary where

import Test.QuickCheck
import Tests

instance Arbitrary Tests where
  arbitrary = do
    tests <- listOf arbitrary
    return $ Tests tests

instance Arbitrary Test where
  arbitrary = do
    name <- arbitrary
    status <- arbitrary
    return $ Test name status

instance Arbitrary TestStatus where
  arbitrary = elements [Passed, NotImplemented, Failed, FailedRegression]
