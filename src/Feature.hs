module Feature where

newtype Features = Features [Feature]

data Feature = Feature { _name :: String
                       , _userStories :: [UserStory]
                       }
data UserStory = UserStory { _userStoryDesc :: String
                           , _criteria :: [Criteria]
                           }

data Criteria = Criteria { _criteriaName :: String
                         , _status :: Status
                         , _steps :: [Step]
                         }

newtype Step = Step String

data Status = Done | NotDone
