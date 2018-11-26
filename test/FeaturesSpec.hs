module FeaturesSpec where

import Truth as Th
import Feature
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

type Acc = ([Feature], Feature)

spec :: Spec
spec = do
  describe "Pandoc transformer" $ do
    it "should return a sample features list" $ do
      sample <- sampleFeatures
      (extractFeaturesFromPandoc sample) `shouldBe` expected
  where
    sampleFeatures :: IO Pandoc
    sampleFeatures = fmap mapRes (PC.runIO $ readMarkdown def (pack Fr.sample))
    mapRes (Left _) = error "Could not read sample data"
    mapRes (Right d) = d
    expected = 
      Features {_features = [
        Feature {_featureName = "Network Launch", _userStories = [
          UserStory {_userStoryDesc = "UC: As a Coop SRE I want to launch a network", _criteria = [
            Criteria {_criteriaName = "AC: A succesful genesis ceremony", _testName = "test: [todo: requires integration test]", _status = Missing, _steps = []},
            Criteria {_criteriaName = "AC: A succesful genesis ceremony with read-only nodes joining", _testName = "test: [todo: requires integration test]", _status = Missing, _steps = []},
            Criteria {_criteriaName = "AC: A NOT succesful genesis ceremony (not enough sigs)", _testName = "test: [todo: requires integration test]", _status = Missing, _steps = []},
            Criteria {_criteriaName = "AC: A validator catching up after ceremony", _testName = "test: [todo: requires integration test]", _status = Missing, _steps = []}]}]},
        Feature {_featureName = "Proof of stake consensus", _userStories = [
          UserStory {_userStoryDesc = "UC: As a dApp developer I want to be able to deploy my rholang contract to a validator", _criteria = [
            Criteria {_criteriaName = "AC: A correct contract gets deployed successfully", _testName = "test: [todo: requires integration test]", _status = Missing, _steps = []},
            Criteria {_criteriaName = "AC: An incorrect contract does not get deployed", _testName = "test: [todo: requires integration test]", _status = Missing, _steps = []}]}]}]}
