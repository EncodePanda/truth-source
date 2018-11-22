module Config where

import Options.Applicative
import Data.Semigroup ((<>))

data Config = Config { _featuresFile :: String
                     , _testsResultsFile :: String
                     } deriving Show


parser :: Parser Config
parser = Config
  <$> strOption
          ( long "features-file"
         <> metavar "FEATURES"
         <> help "Markdown file holding features" )
  <*> strOption
          ( long "test-file"
         <> metavar "TESTS"
         <> help "JSON file holding integration tests results" )

