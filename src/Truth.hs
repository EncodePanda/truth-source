{-# LANGUAGE OverloadedStrings #-}
module Truth where

import Config
import Feature
import Tests
import Data.Text (Text, pack)
import Text.Pandoc.Options
import Text.Pandoc.Class
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Definition
import Control.Monad.IO.Class
import Control.Lens
import Control.Lens.Each

generate :: (PandocMonad m, MonadIO m) => Config -> m Pandoc
generate config = do
  doc        <- readFeaturesDoc $ config^.featuresFile
  features   <- loadFeatures doc
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

loadFeatures :: Pandoc -> m Features
loadFeatures = undefined

combine :: Features -> Tests -> Features
combine fs (Tests []) = fs & features.traverse.userStories.traverse.criteria.traverse.status .~  Missing
combine fs ts = fs

toDoc :: Features -> Pandoc
toDoc features = undefined
