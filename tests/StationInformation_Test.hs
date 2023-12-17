module StationInformation_Test (calculateDistanceTest, getClosestStationsTest) where

import StationInformation (StationInformation (..), calculateDistance, getClosestStations)
import Test.HUnit (Test (TestCase), assertEqual, (~:))
import UserPos (UserPos (..))

stations :: [StationInformation]
stations =
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
      },
    StationInformation
      { station_id = "526",
        name = "Lille Grensen",
        address = "Lille Grensen - Akersgata 43",
        lat = 59.9138973,
        lon = 10.7423101
      }
  ]

userPos :: UserPos
userPos = UserPos {lat = 59.91684038468469, lon = 10.746329853985955}

calculateDistanceTest :: Test
calculateDistanceTest =
  "calculateDistanceTest" ~: do
    let calc = calculateDistance userPos
        calculated = map calc stations
    assertEqual "calculateDistanceTest: " [2160.9958, 601.14197, 396.43002] calculated

getClosestStationsTest :: Test
getClosestStationsTest =
  "getClosestStationsTest" ~: do
    -- \$ applies the function to the left to everything to the right
    let closest_stations = getClosestStations userPos stations $ Just 2
    assertEqual "getClosestStationsTest: " ["526", "527"] (map station_id closest_stations)
