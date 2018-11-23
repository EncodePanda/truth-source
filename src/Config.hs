module Config where

import Options.Applicative
import Data.Semigroup ((<>))

data Config = Config { _features :: String
                     , _tests :: String
                     , _output :: String
                     } deriving Show


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

