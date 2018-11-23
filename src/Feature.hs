module Feature where

newtype Features = Features [Feature]
  deriving (Show, Eq)

data Feature = Feature { _name :: String
                       , _userStories :: [UserStory]
                       } deriving (Show, Eq)
data UserStory = UserStory { _userStoryDesc :: String
                           , _criteria :: [Criteria]
                           } deriving (Show, Eq)

data Criteria = Criteria { _criteriaName :: String
                         , _status :: Status
                         , _steps :: [Step]
                         } deriving (Show, Eq)

newtype Step = Step String
  deriving (Show, Eq)

data Status = Done | NotDone
  deriving (Show, Eq)
