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
        extractOutcome `fmap` T.parseRaw Raw.report_one_failed `shouldBe` Just ["failed"]
    it "should read a file report with mixed results" $ do
        extractOutcome `fmap` T.parseRaw Raw.report_mixed `shouldBe` Just ["passed", "skipped", "skipped", "passed", "failed", "failed"]
    where
        extractOutcome v = v ^.. includedLens . traverse . attributesLens . outcomeLens
