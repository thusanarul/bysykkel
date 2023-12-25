{-# LANGUAGE DuplicateRecordFields #-}

module StationStatus_Test (findAvailabilityForStationTest) where

import StationInformation as SI (StationInformation (..))
import StationStatus as SS (StationStatus (..), findAvailabilityForStation, findAvailabilityForStations)
import Test.HUnit (Test (TestCase), assertEqual, (~:))

stations :: [StationStatus]
stations =
  [ StationStatus
      { is_installed = True,
        is_renting = True,
        num_bikes_available = 3,
        num_docks_available = 20,
        last_reported = 1703012317,
        is_returning = True,
        station_id = "561"
      },
    StationStatus
      { is_installed = True,
        is_renting = True,
        num_bikes_available = 1,
        num_docks_available = 10,
        last_reported = 1703012317,
        is_returning = True,
        station_id = "527"
      },
    StationStatus
      { is_installed = True,
        is_renting = True,
        num_bikes_available = 0,
        num_docks_available = 15,
        last_reported = 1703012317,
        is_returning = True,
        station_id = "526"
      }
  ]

stationIds :: [StationInformation]
stationIds =
  [ StationInformation
      { station_id = "561",
        name = "Kirkeveien",
        address = "Kirkeveien 106A",
        lat = 59.93347989334691,
        lon = 10.726294394131088
      },
    StationInformation
      { station_id = "527",
        name = "Biskop Gunnerus' gate",
        address = "Oslo City - Biskop Gunnerus' gate",
        lat = 59.9123341,
        lon = 10.752292
      }
  ]

station :: StationInformation
station = head stationIds

findAvailabilityForStationTest :: Test
findAvailabilityForStationTest =
  "findAvailabilityForStationTest" ~: do
    let expected = Just $ head stations
        actual = findAvailabilityForStation stations (SI.station_id station)
    assertEqual "findAvailabilityForStationTest: " expected actual

findAvailabilityForStationsTest :: Test
findAvailabilityForStationsTest =
  "findAvailabilityForStationTest" ~: do
    let expected = take 2 stations
        actual = findAvailabilityForStations stations (map SI.station_id stationIds)
    assertEqual "findAvailabilityForStationsTest: " expected actual
