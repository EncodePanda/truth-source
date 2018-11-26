{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Tests where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Lens.TH
import Control.Lens
import Control.Lens.Reified
import Text.Show.Functions

data Report = Report {
    included :: [TestReport]
} deriving (Eq, Show, Generic)

data TestReport = TestReport {
    id :: Int,
    attributes :: Attributes
} deriving (Eq, Show, Generic)

data Attributes = Attributes {
    name :: String,
    outcome :: String,
    setup :: Maybe Setup
} deriving (Eq, Show, Generic)

data Setup = Setup {
    longrepr :: Maybe String
} deriving (Eq, Show, Generic)

makeLensesFor [("included", "includedLens")] ''Report
makeLensesFor [("attributes", "attributesLens")] ''TestReport
makeLensesFor [("outcome", "outcomeLens"), ("name", "nameLens"), ("setup", "setupLens")] ''Attributes
makeLensesFor [("longrepr", "longreprLens")] ''Setup

data Tests = Tests [Test]
  deriving (Show, Eq)
data Test = Test String TestStatus
  deriving (Show, Eq)

data TestStatus = Passed | NotImplemented | Failed | FailedRegression | Unknown deriving (Eq, Show)
  deriving (Show, Eq)

instance ToJSON TestReport
instance FromJSON TestReport

instance ToJSON Attributes
instance FromJSON Attributes

instance ToJSON Setup
instance FromJSON Setup

instance ToJSON Report
instance FromJSON Report

parse :: String -> Maybe Tests
parse s = case parseRaw s of
    Nothing -> Nothing
    Just r -> Just $ transformToTest r

transformToTest :: Report -> Tests
transformToTest r = 
    Tests $ map mapResult (attrs r)
    where
        mapStatus s = case s of
            "skipped" -> NotImplemented
            "passed"  -> Passed
            "failed"  -> Failed
            _         -> Unknown
        mapResult :: Attributes -> Test
        mapResult t = Test (name t) (mapStatus (outcome t))
        attrs r = r ^.. includedLens . traverse . attributesLens

parseRaw :: String -> Maybe Report
parseRaw s = decode $ C.pack s
