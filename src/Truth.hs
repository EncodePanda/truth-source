{-# LANGUAGE OverloadedStrings #-}
module Truth where

import Config
import Feature
import Tests
import Data.Text (Text, pack)
import Text.Pandoc
import Text.Pandoc.Options
import Text.Pandoc.Class
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Definition
import Control.Monad.IO.Class
import Control.Lens
import Control.Lens.Each
import Data.Foldable

generate :: (PandocMonad m, MonadIO m) => Config -> m Pandoc
generate config = do
  doc        <- readFeaturesDoc $ config^.featuresFile
  features   <- return $ loadFeatures doc
  tests      <- readTests $ config^.testsFile
  let result = combine features tests
  return $ toDoc result

readFeaturesDoc :: (PandocMonad m, MonadIO m) => String -> m Pandoc
readFeaturesDoc path = do
  raw <- liftIO $ readFile $ path
  readMarkdown def (pack raw)

readTests :: (MonadIO m) => String -> m Tests
readTests path = do
  raw <- liftIO $ readFile $ path
  return $ parseReport (parse raw)
  where
    parseReport :: Maybe Tests -> Tests
    parseReport (Nothing) = error "could not parse report"
    parseReport (Just ts) = ts

loadFeatures :: Pandoc -> Features
loadFeatures p = extractFeaturesFromPandoc p

combine :: Features -> Tests -> Features
combine fs (Tests []) = fs & features.traverse.userStories.traverse.criteria.traverse.status .~  Missing
combine fs ts = over allCriteria (fmap (modify (ts^.tests))) fs
  where
     modify :: [Test] -> Criteria -> Criteria
     modify ts c = case (findTest ts c) of
       Nothing  -> status .~ Missing $ c
       (Just t) -> status .~ (testToStatus (t^.testStatus)) $ c

     testToStatus Passed = Done
     testToStatus NotImplemented = NotDone "not implemented"
     testToStatus Failed = NotDone "failed"
     testToStatus Regression = NotDone "regression"
     testToStatus Unknown = NotDone "unknown"

     findTest :: [Test] -> Criteria -> Maybe Test
     findTest ts c = find (\t -> t^.testDesc == c^.testName) ts

summary :: Features -> Pandoc
summary features = undefined

details :: Features -> Pandoc
details (Features fs) = Pandoc nullMeta (fmap feature2Name fs)
  where
    feature2Name :: Feature -> Block
    feature2Name f = Header 2 nullAttr [Str (f^.featureName)]

toDoc :: Features -> Pandoc
toDoc features = summary features <> details features
