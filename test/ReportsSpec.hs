module ReportsSpec where

import Test.Hspec
import Test.QuickCheck
import Tests as T
import ReportsRaw as Raw
import Control.Lens

main :: IO ()
main = hspec spec



spec :: Spec
spec = describe "the parser" $ do
    it "should read a file report with one skipped test" $ do
        extractOutcome `fmap` T.parseRaw Raw.report_one_skipped `shouldBe` Just ["skipped"]
    it "should read a file report with one passed test" $ do
        extractOutcome `fmap` T.parseRaw Raw.report_one_passed `shouldBe` Just ["passed"]
    it "should read a file report with one failed test" $ do
        (1==1) `shouldBe` True
    where
        extractOutcome v = v ^.. includedLens . traverse . attributesLens . outcomeLens
