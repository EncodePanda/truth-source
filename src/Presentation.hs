{-# LANGUAGE OverloadedStrings #-}
module Presentation where

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

present :: (MonadIO m) => Features -> m Pandoc
present features = pure $ toDoc features

summary :: Features -> Pandoc
summary (Features []) = Pandoc nullMeta [Null]
summary features = Pandoc nullMeta [Table [Str "Features"] [AlignLeft] [0] [] []]

details :: Features -> Pandoc
details (Features fs) = Pandoc nullMeta (fmap feature2Name fs)
  where
    feature2Name :: Feature -> Block
    feature2Name f = Header 2 nullAttr [Str (f^.featureName)]

toDoc :: Features -> Pandoc
toDoc features = summary features <> details features
