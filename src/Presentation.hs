{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Presentation where

import GHC.Generics
import Config
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
import Text.RawString.QQ
import Control.Monad.IO.Class
import Control.Lens
import Control.Lens.Each
import Data.Foldable

present :: (MonadIO m) => Features -> m Pandoc
present features = pure $ toDoc features

summary :: Features -> Pandoc
summary (Features []) = Pandoc nullMeta [P.Null]
summary features = Pandoc nullMeta [Table [Str "Features"] [AlignLeft] [0] [] []]

details :: Features -> Pandoc
details (Features fs) = Pandoc nullMeta (fmap feature2Name fs)
  where
    feature2Name :: Feature -> Block
    feature2Name f = Header 2 nullAttr [Str (f^.featureName)]

toDoc :: Features -> Pandoc
toDoc features = summary features <> details features

class Completed a where
  klass :: a -> String
  label :: a -> String

instance Completed Feature where
  klass f = "success"
  label f = "completed"

instance ToJSON Features where
  toJSON a = object [
    "features" AT..= map toJSON (_features a)
    ]

instance ToJSON Feature where
  toJSON a = object [
    "featureName" AT..= _featureName a ,
    "class" AT..= klass a,
    "label" AT..= label a]

instance ToJSON UserStory
instance ToJSON Criteria
instance ToJSON Step
instance ToJSON Status

template :: Text
template = [r|
"Features:
<table style="width:100%">
  $for(features)$
  <h3>$features.featureName$ <span class="badge badge-$features.class$">$features.label$</span></h3>
  $endfor$
</table>
|]

templateIO :: IO Text
templateIO = fmap pack $ readFile "template/template.html"

template' :: Features -> IO ()
template' f = do
  template <- templateIO
  case compileTemplate template of
         Left e    -> error e
         Right t   -> putStrLn $ renderTemplate t $ f
