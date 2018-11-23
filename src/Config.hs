{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import Control.Lens.TH
import GHC.Generics
import Options.Applicative
import Data.Semigroup ((<>))

data Config = Config { _featuresFile :: String
                     , _testsFile :: String
                     , _outputFile :: String
                     } deriving Show

makeLenses ''Config

parser :: Parser Config
parser = Config
  <$> strOption
          ( long "features"
         <> metavar "FEATURES"
         <> short 'f'
         <> help "Markdown file holding features" )
  <*> strOption
          ( long "tests"
         <> short 't' 
         <> metavar "TESTS"
         <> help "JSON file holding integration tests results" )
  <*> strOption
          ( long "output"
         <> metavar "OUTPUT"
         <> short 'o'
         <> value "output.html"
         <> help "Path to HTML output file" )

