{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Tests where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Lens.TH

data Report = Report {
    included :: [TestReport]
} deriving (Eq, Show, Generic)

data TestReport = TestReport {
    id :: Int,
    attributes :: Attributes
} deriving (Eq, Show, Generic)

data Attributes = Attributes {
    outcome :: String
} deriving (Eq, Show, Generic)

makeLensesFor [("included", "includedLens")] ''Report
makeLensesFor [("attributes", "attributesLens")] ''TestReport
makeLensesFor [("outcome", "outcomeLens")] ''Attributes

data Tests = Tests [Test]
  deriving (Show, Eq)
data Test = Test String TestStatus
  deriving (Show, Eq)

data TestStatus = Passed | NotImplemented | Failed | FailedRegression
  deriving (Show, Eq)

instance ToJSON TestReport
instance FromJSON TestReport

instance ToJSON Attributes
instance FromJSON Attributes

instance ToJSON Report
instance FromJSON Report

parse :: String -> Tests
parse = undefined

parseRaw :: String -> Maybe Report
parseRaw s = decode $ C.pack s
