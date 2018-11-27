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

instance ToJSON Features where
  toJSON a = object ["features" AT..=
     map toJSON (_features a)
    ]

instance ToJSON Feature where
  toJSON a = object ["featureName" AT..= _featureName a ]

instance ToJSON UserStory
instance ToJSON Criteria
instance ToJSON Step
instance ToJSON Status

template :: Text
template = [r|
"Features:
<table style="width:100%">
  <tr>
    <th>Feature name</th>
  </tr>
  $for(features)$
  <tr>
    <td>$features.featureName$</td>
  </tr>
  $endfor$
</table>
|]


template' :: Features -> IO ()
template' f = case compileTemplate template of
         Left e    -> error e
         Right t   -> putStrLn $ renderTemplate t $ f
