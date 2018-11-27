{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Presentation where

import GHC.Generics
import Config
import Crypt
import Feature
import Tests
import Data.Aeson
import Data.Aeson.Types as AT
import Data.Text (Text, pack)
import Text.DocTemplates
import Text.Pandoc as P
import Text.Pandoc.Options
import Text.Pandoc.Class
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Definition
import Control.Monad.IO.Class
import Control.Lens
import Control.Lens.Each
import Data.Foldable

type CompletedStatus = (Bool, (Int, Int))

class Completed a where
  isCompleted :: a -> CompletedStatus
  klazz :: a -> String
  klazz a
    | fst $ isCompleted a = "success"
    | otherwise = "danger"
  label :: a -> String
  label a
    | fst $ isCompleted a = "completed"
    | otherwise = (show . isCompleted) a

instance Completed Feature where
  isCompleted (Feature _ []) = (False, (0, 0))
  isCompleted (Feature _ us) = isCompleted us
  klazz (Feature _ []) = "danger"
  klazz (Feature _ us) = klazz us
  label (Feature _ []) = "not defined"
  label (Feature _ us) = label us

instance Completed UserStory where
  isCompleted (UserStory _ []) = (False, (0, 0))
  isCompleted (UserStory _ cs) = isCompleted cs
  klazz (UserStory _ []) = "danger"
  klazz (UserStory _ cs) = klazz cs
  label (UserStory _ []) = "not defined"
  label (UserStory _ cs) = label cs

instance Completed a => Completed [a] where
  isCompleted as = (done as == all as, (done as, all as))
    where
      done as = length $ filter (fst . isCompleted) as
      all as = length as

instance Completed Criteria where
  isCompleted (Criteria _ _ Done _ ) = (True, (1, 1))
  isCompleted _ = (False, (0, 1))

instance ToJSON Features where
  toJSON a = object [
    "features" AT..= map toJSON (_features a)
    ]

instance ToJSON Feature where
  toJSON a = object [
    "id" AT..= encodeToString (show a),
    "featureName" AT..= _featureName a ,
    "userStories" AT..= map toJSON (_userStories a),
    "class" AT..= klazz a,
    "label" AT..= label a]

instance ToJSON UserStory where
  toJSON a = object [
    "id" AT..= encodeToString (show a),
    "description" AT..= _userStoryDesc a,
    "criteria" AT..= map toJSON (_criteria a),
    "class" AT..= klazz a,
    "label" AT..= label a]

instance ToJSON Criteria where
  toJSON a = object [
    "id" AT..= encodeToString (show a),
    "name" AT..= _criteriaName a,
    "steps" AT..= _steps a,
    "class" AT..= klazz a,
    "label" AT..= label a]

instance ToJSON Step where
  toJSON a = object [
    "id" AT..= encodeToString (show a),
    "value" AT..= show a]

instance ToJSON Status

templateIO :: IO Text
templateIO = fmap pack $ readFile "template/template.html"

present :: Features -> IO String
present f = do
  template <- templateIO
  case compileTemplate template of
         Left e    -> error e
         Right t   -> return $ renderTemplate t $ f
