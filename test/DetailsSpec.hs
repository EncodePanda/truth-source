{-# LANGUAGE QuasiQuotes #-}
module DetailsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Lens
import Feature
import Tests
import Truth
import qualified Text.Pandoc as PC
import Data.Text
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Options
import NeatInterpolation

spec :: Spec
spec =
  describe "details" $ do
    it "temp" $ do
      pandoc <- fmap mapRes (PC.runIO $ readFeaturesDoc "example.md")
      putStrLn $ show pandoc

    it "should return a single feature as h2 (no white spaces)" $ do
      checkDetails (Features [Feature "Name" []]) [text|
        ## Name
      |]

    it "should serialize a sample report" $ do
      let sampleReport = Features [Feature "F1" [UserStory "UC1" [Criteria "AC1" "test1" Missing [Step "step1", Step "step2"]]]]

      checkDetails sampleReport [text|
        ## F1

        ### UC: UC1

        #### AC: AC1

        ##### test: test1 - MISSING

        ##### steps:
        * step1
        * step2
      |]

    where
      checkDetails :: Features -> Text -> IO ()
      checkDetails features expected = do
        expectedPandoc <- parseMarkdown expected
        details features `shouldBe` expectedPandoc

      parseMarkdown :: Text -> IO PC.Pandoc
      parseMarkdown content = fmap mapRes (PC.runIO $ readMarkdown def content)

      mapRes (Left _) = error "blee"
      mapRes (Right d) = d

