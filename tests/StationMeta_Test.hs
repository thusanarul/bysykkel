module StationMeta_Test (mergeTest) where

import StationInformation as SI (StationInformation (..))
import StationMeta as SM (StationMeta (..), StationMetaAvailability (..), StationMetaInformation (..), merge)
import StationStatus as SS (StationStatus (..))
import Test.HUnit (Test, assertEqual, (~:))
import Test.HUnit.Base

sis :: [StationInformation]
sis =
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

sss :: [StationStatus]
sss =
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

sms :: [StationMeta]
sms =
  [ StationMeta
      { station_id = "561",
        information =
          StationMetaInformation
            { name = "Kirkeveien",
              address = "Kirkeveien 106A",
              lat = 59.93347989334691,
              lon = 10.726294394131088
            },
        availability =
          StationMetaAvailability
            { num_bikes_available = 3,
              num_docks_available = 20,
              last_reported = 1703012317
            }
      },
    StationMeta
      { station_id = "527",
        information =
          StationMetaInformation
            { name = "Biskop Gunnerus' gate",
              address = "Oslo City - Biskop Gunnerus' gate",
              lat = 59.9123341,
              lon = 10.752292
            },
        availability =
          StationMetaAvailability
            { num_bikes_available = 1,
              num_docks_available = 10,
              last_reported = 1703012317
            }
      },
    StationMeta
      { station_id = "526",
        information =
          StationMetaInformation
            { name = "Lille Grensen",
              address = "Lille Grensen - Akersgata 43",
              lat = 59.9138973,
              lon = 10.7423101
            },
        availability =
          StationMetaAvailability
            { num_bikes_available = 0,
              num_docks_available = 15,
              last_reported = 1703012317
            }
      }
  ]

sisPerm :: [StationInformation]
sisPerm = take 2 sis ++ [head sis]

mergeTest :: [Test]
mergeTest =
  [ "mergeTest1" ~: do
      let metas = zipWith merge sis sss
      sms ~=? metas,
    "mergeTest2" ~: do
      let metas = zipWith merge sisPerm sss
      sms ~=? metas
  ]