module ReportsSpec where

import Test.Hspec
import Test.QuickCheck
import Tests as T
import ReportsRaw as Raw
import Control.Lens

spec :: Spec
spec = 
    do
        describe "Parser" $ do
            it "should read a file report with one skipped test" $ do
                extractOutcome `fmap` T.parseRaw Raw.report_one_skipped `shouldBe` Just ["skipped"]
            it "should read the reason for skipping a test" $ do
                extractLongrepr `fmap` T.parseRaw Raw.report_one_skipped `shouldBe` Just [ Just "('test/test_heterogenous_validators.py', 110, 'Skipped: https://rchain.atlassian.net/browse/CORE-1455')" ]
            it "should read a file report with one passed test" $ do
                extractOutcome `fmap` T.parseRaw Raw.report_one_passed `shouldBe` Just ["passed"]
            it "should read a passed reports' test name" $ do
                extractName `fmap` T.parseRaw Raw.report_one_passed `shouldBe` Just ["test/test_propose.py::test_propose"]
            it "should read a file report with one failed test" $ do
                extractOutcome `fmap` T.parseRaw Raw.report_one_failed `shouldBe` Just ["failed"]
            it "should read a file report with mixed results" $ do
                extractOutcome `fmap` T.parseRaw Raw.report_mixed `shouldBe` Just ["passed", "skipped", "skipped", "passed", "failed", "failed"]
        describe "Reader" $ do
            it "should map report to tests" $ do
                extractStatuses `fmap` (T.parse Raw.report_mixed) `shouldBe` Just [Passed,NotImplemented,NotImplemented,Passed,Failed,Failed]
    where
        extractOutcome v = v ^.. includedLens . traverse . attributesLens . outcomeLens
        extractLongrepr v = v ^.. includedLens . traverse . attributesLens . setupLens . traverse . longreprLens
        extractName v = v ^.. includedLens . traverse . attributesLens . nameLens
        extractStatuses (Tests ts) = map (\v -> case v of (Test _ status) -> status) ts


