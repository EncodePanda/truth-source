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

extractFeaturesFromPandoc (Pandoc meta heads) = extractFeatures heads
  where
    extractFeatures :: [Block] -> Features
    extractFeatures l = buildFeatureTrees (takeWhile lowerLevel (tail (dropWhile (\v -> (not (featureReq v))) l)))
    featureReq :: Block -> Bool
    featureReq (Header 1 _ [Str "Mainnet",Space,Str "Feature",Space,Str "Requirements"]) = True
    featureReq _ = False
    lowerLevel (Header 1 _ _) = False
    lowerLevel _ = True
    wrapUpFeatures (acc, last) = Features (acc ++ [last])
    buildFeatureTrees l =  wrapUpFeatures (foldl mapper ([], makeFeature (head l)) (tail l))
    makeFeature (Header 2 _ l) = Feature (printInline l) []
    makeUserStory (Header 3 _ l) = UserStory (printInline l) []
    makeCriteria (Header 4 _ l) = Criteria (printInline l) "not available" Missing []
    addNameToCriteriaInUserStory (UserStory storyName crits) name = (UserStory storyName ((init crits) ++ [(addNameToCriteria (last crits) name)]))
    appendCriteria (UserStory n crs) c = UserStory n (crs ++ [c])
    addNameToCriteria (Criteria cn tn st s) name = Criteria cn name st s
    mapper (l, c) h@(Header 2 _ _) = (l ++ [c], makeFeature h)
    mapper (l, (Feature n []) ) h@(Header 3 _ _) = (l, Feature n [makeUserStory h])
    mapper (l, (Feature n ucs) ) h@(Header 3 _ _) = (l, Feature n (ucs ++ [makeUserStory h]))
    mapper (l, (Feature n ucs) ) h@(Header 4 _ _) = (l, Feature n ((init ucs) ++ [(appendCriteria (last ucs) (makeCriteria h))]))
    mapper (l, (Feature n ucs) ) h@(Header 5 _ (line@((Str "test:"):rest))) = (l, Feature n ((init ucs) ++ [(addNameToCriteriaInUserStory (last ucs) (printInline line))]))
    mapper (l, f) h@(Header 5 _ (line@((Str "steps:"):rest))) = (l, f)
    mapper (l, f) (BulletList _) = (l, f)
    mapper (l, f) _ = (l, f)
    toStringInline (Str s) = s
    toStringInline Space = " "
    toStringInline a = show a
    printInline :: [Inline] -> String
    printInline l = intercalate "" (map toStringInline l)
