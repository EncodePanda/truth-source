{-# LANGUAGE OverloadedStrings #-}
module Truth where

import Data.Text (Text, pack)
import Text.Pandoc.Options
import Text.Pandoc.Class
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Definition
import Control.Monad.IO.Class
import Config

generate :: (PandocMonad m, MonadIO m) => Config -> m Pandoc
generate config = do
  features <- liftIO $ readFile $ _features config
  readMarkdown def (pack features)
