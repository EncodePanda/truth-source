{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Feature where

import Control.Lens
import Control.Lens.TH
import GHC.Generics
import Text.Pandoc.Options
import Text.Pandoc.Class as PC
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Definition
import Data.List
import Control.Monad

data Features = Features { _features :: [Feature]}
  deriving (Show, Eq, Generic)

data Feature = Feature { _featureName :: String
                       , _userStories :: [UserStory]
                       } deriving (Show, Eq, Generic)

data UserStory = UserStory { _userStoryDesc :: String
                           , _criteria :: [Criteria]
                           } deriving (Show, Eq, Generic)

data Criteria = Criteria { _criteriaName :: String
                         , _testName :: String
                         , _status :: Status
                         , _steps :: [Step]
                         } deriving (Show, Eq, Generic)

newtype Step = Step String
  deriving (Show, Eq, Generic)

data Status = Done | NotDone String | Missing
  deriving (Show, Eq, Generic)

makeLenses ''Features
makeLenses ''Feature
makeLenses ''UserStory
makeLenses ''Criteria

allCriteria :: Traversal' Features [Criteria]
allCriteria = features.traverse.userStories.traverse.criteria

extractFeaturesFromPandoc :: Pandoc -> Features
extractFeaturesFromPandoc (Pandoc meta heads) = buildFeatureTrees (getMainnetFeatureReqs heads)
  where
    getMainnetFeatureReqs :: [Block] -> [Block]
    getMainnetFeatureReqs l = takeWhile lowerLevel (tail (dropWhile (\v -> (not (featureReq v))) l))

    wrapUpFeatures :: ([Feature], Feature) -> Features
    wrapUpFeatures (acc, last) = Features (acc ++ [last])

    buildFeatureTrees :: [Block] -> Features
    buildFeatureTrees l = wrapUpFeatures (foldl mapBlockToFeature ([], makeFeature (head l)) (tail l))

    featureReq :: Block -> Bool
    featureReq (Header 1 _ [Str "Mainnet",Space,Str "Feature",Space,Str "Requirements"]) = True
    featureReq _ = False

    lowerLevel :: Block -> Bool
    lowerLevel (Header 1 _ _) = False
    lowerLevel _ = True

makeFeature :: Block -> Feature
makeFeature (Header 2 _ l) = Feature (printInline l) []

makeUserStory :: Block -> UserStory
makeUserStory (Header 3 _ l) = UserStory (printInline l) []

makeCriteria :: Block -> Criteria
makeCriteria (Header 4 _ l) = Criteria (printInline l) "not available" Missing []

mapBlockToFeature :: ([Feature], Feature) -> Block -> ([Feature], Feature)
mapBlockToFeature (l, c) h@(Header 2 _ _) = (l ++ [c], makeFeature h)
mapBlockToFeature (l, (Feature n []) ) h@(Header 3 _ _) = (l, Feature n [makeUserStory h])
mapBlockToFeature (l, (Feature n ucs) ) h@(Header 3 _ _) = (l, Feature n (ucs ++ [makeUserStory h]))
mapBlockToFeature (l, (Feature n ucs) ) h@(Header 4 _ _) = (l, Feature n ((init ucs) ++ [(appendCriteria (last ucs) (makeCriteria h))]))
mapBlockToFeature (l, (Feature n ucs) ) h@(Header 5 _ (line@((Str "test:"):Space:rest))) = (l, Feature n ((init ucs) ++ [(addNameToCriteriaInUserStory (last ucs) (printInline rest))]))
mapBlockToFeature (l, f) h@(Header 5 _ (line@((Str "steps:"):rest))) = (l, f)
mapBlockToFeature (l, (Feature n ucs) ) (BulletList elems) = (l, Feature n ((init ucs) ++ [(addStepsToCriteriaInUserStory (last ucs) elems)]))
mapBlockToFeature (l, f) _ = (l, f)

appendCriteria :: UserStory -> Criteria -> UserStory
appendCriteria (UserStory n crs) c = UserStory n (crs ++ [c])

addNameToCriteriaInUserStory :: UserStory -> String -> UserStory
addNameToCriteriaInUserStory (UserStory storyName crits) name = (UserStory storyName ((init crits) ++ [(addNameToCriteria (last crits) name)]))
  where
    addNameToCriteria :: Criteria -> String -> Criteria
    addNameToCriteria (Criteria cn tn st s) name = Criteria cn name st s

addStepsToCriteriaInUserStory :: UserStory -> [[Block]] -> UserStory
addStepsToCriteriaInUserStory (UserStory n crs) blocks = UserStory n ((init crs) ++ [addStepsToCriteria (last crs) (blocksToSteps blocks)])
  where
    addStepsToCriteria :: Criteria -> [Step] -> Criteria
    addStepsToCriteria (Criteria cn tn st _) sts = Criteria cn tn st sts
    blocksToSteps :: [[Block]] -> [Step]
    blocksToSteps (a:rest) = (map Step (printBlocks a)) ++ (blocksToSteps rest)
    blocksToSteps ([]) = []

toStringInline :: Inline -> String
toStringInline (Str s) = s
toStringInline (Code _ s) = s
toStringInline (Link _ ins t) = printInline ins
toStringInline (Emph ins) = "_" ++ (printInline ins) ++ "_"
toStringInline (Strong ins) = "**" ++ (printInline ins) ++ "**"
toStringInline Space = " "
toStringInline a = show a

printInline :: [Inline] -> String
printInline l = intercalate "" (map toStringInline l) 

printBlock :: Block -> String
printBlock (Plain inlines) = printInline inlines
printBlock b = show b

printBlocks :: [Block] -> [String]
printBlocks (x:xs) = printBlock x : printBlocks xs
printBlocks [] = []
