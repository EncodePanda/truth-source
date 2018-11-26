module FeaturesSpec where

import Truth as Th
import Feature as F
import FeaturesRaw as Fr
import Test.Hspec
import Test.QuickCheck
import Tests as T
import FeaturesRaw as Raw
import Control.Lens
import Text.Pandoc.Options
import Text.Pandoc.Class as PC
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Definition
import Data.Text (pack)

spec :: Spec
spec = do
    describe "Pandoc transformer" $ do
        it "should return a sample features list" $ do
            sample <- sampleFeatures
            Th.loadFeatures sample `shouldBe` F.Features []
    where
        sampleFeatures :: IO Pandoc -- = 
        sampleFeatures = fmap mapRes (PC.runIO $ readMarkdown def (pack Fr.sample))
        mapRes (Left _) = error "blee"
        mapRes (Right d) = d