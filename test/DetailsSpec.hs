{-# LANGUAGE QuasiQuotes #-}
module DetailsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Lens
import Feature
import Tests
import Truth
import Presentation
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

    where
      checkDetails :: Features -> Text -> IO ()
      checkDetails features expected = do
        expectedPandoc <- parseMarkdown expected
        details features `shouldBe` expectedPandoc

      parseMarkdown :: Text -> IO PC.Pandoc
      parseMarkdown content = fmap mapRes (PC.runIO $ readMarkdown def content)

      mapRes (Left _) = error "blee"
      mapRes (Right d) = d
