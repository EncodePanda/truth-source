{-# LANGUAGE QuasiQuotes #-}
module DetailsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Lens
import Feature
import Tests
import Truth
import qualified Text.Pandoc as PC
import DetailsRaw
import Data.Text
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Options
import Text.RawString.QQ

singleFeature = 
    [r|
## Name
    |]

spec :: Spec
spec =
  describe "details" $ do
    it "temp" $ do
      pandoc <- fmap mapRes (PC.runIO $ readFeaturesDoc "example.md")
      putStrLn $ show pandoc

    it "should return a single feature as h2 (no white spaces)" $ do
      temp <- sampleFeatures singleFeature
      let fs = Features [Feature "Name" []]
      details fs `shouldBe` temp
  
    where
      sampleFeatures :: String -> IO PC.Pandoc
      sampleFeatures content = fmap mapRes (PC.runIO $ readMarkdown def (pack content))
      
      mapRes (Left _) = error "blee"
      mapRes (Right d) = d

