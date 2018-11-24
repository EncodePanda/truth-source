{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Feature where

import Control.Lens.TH
import GHC.Generics

data Features = Features { _features :: [Feature]}
  deriving (Show, Eq)

data Feature = Feature { _featureName :: String
                       , _userStories :: [UserStory]
                       } deriving (Show, Eq, Generic)
data UserStory = UserStory { _userStoryDesc :: String
                           , _criteria :: [Criteria]
                           } deriving (Show, Eq, Generic)

data Criteria = Criteria { _criteriaName :: String
                         , _status :: Status
                         , _steps :: [Step]
                         } deriving (Show, Eq, Generic)

newtype Step = Step String
  deriving (Show, Eq, Generic)

data Status = Done | NotDone | Missing
  deriving (Show, Eq, Generic)

makeLenses ''Features
makeLenses ''Feature
makeLenses ''UserStory
makeLenses ''Criteria
