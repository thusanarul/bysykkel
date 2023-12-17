module Main where

import StationInformation_Test (calculateDistanceTest, getClosestStationsTest)
import System.Exit
import Test.HUnit
  ( Counts (errors, failures),
    Test (TestCase),
    Testable (test),
    assertEqual,
    runTestTT,
  )

test1 :: Test
test1 = TestCase (assertEqual "should work" 1 1)

main :: IO ()
main = do
  result <-
    runTestTT
      ( test
          [ calculateDistanceTest,
            getClosestStationsTest
          ]
      )
  if errors result + failures result == 0
    then exitSuccess
    else exitFailure
