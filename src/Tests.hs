module Tests where

data Report = Report {
                     name :: String
                     }
data Tests = Tests [Test]

data Test = Test String TestStatus

data TestStatus = Passed | NotImplemented | Failed | FailedRegression

