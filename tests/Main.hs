module Main where

import StationInformation_Test (calculateDistanceTest, getClosestStationsTest)
import StationMeta_Test (mergeTest)
import StationStatus_Test (findAvailabilityForStationTest)
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
          ( [ calculateDistanceTest,
              getClosestStationsTest,
              findAvailabilityForStationTest
            ]
              ++ mergeTest
          )
      )
  if errors result + failures result == 0
    then exitSuccess
    else exitFailure
