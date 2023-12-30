{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StationMeta (merge, mergeAll, StationMeta (..), StationMetaAvailability (..), StationMetaInformation (..)) where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (toJSON)
import Data.List (sort)
import GHC.Generics
import StationInformation as SI (StationInformation (..))
import StationStatus as SS (StationStatus (..))

data StationMeta = StationMeta
  { station_id :: String,
    information :: StationMetaInformation,
    availability :: StationMetaAvailability
  }
  deriving (Show, Generic)

instance Eq StationMeta where
  (==) :: StationMeta -> StationMeta -> Bool
  (==) a b = a.station_id == b.station_id

instance Ord StationMeta where
  compare :: StationMeta -> StationMeta -> Ordering
  compare a b = compare a.station_id b.station_id

data StationMetaInformation = StationMetaInformation
  { name :: String,
    address :: String,
    lat :: Float,
    lon :: Float
  }
  deriving (Show, Generic, Eq)

instance ToJSON StationMetaInformation

data StationMetaAvailability = StationMetaAvailability
  { num_bikes_available :: Int,
    num_docks_available :: Int,
    last_reported :: Int
  }
  deriving (Show, Generic, Eq)

instance ToJSON StationMetaAvailability

instance ToJSON StationMeta

merge :: StationInformation -> StationStatus -> StationMeta
merge si ss =
  StationMeta
    { station_id = SI.station_id si,
      information =
        StationMetaInformation
          { name = SI.name si,
            address = SI.address si,
            lat = SI.lat si,
            lon = SI.lon si
          },
      availability =
        StationMetaAvailability
          { num_bikes_available = SS.num_bikes_available ss,
            num_docks_available = SS.num_docks_available ss,
            last_reported = SS.last_reported ss
          }
    }

mergeAll :: [StationInformation] -> [StationStatus] -> [StationMeta]
mergeAll si ss = zipWith merge (filter (elemInStatusList ss) si) ss

elemInStatusList :: [StationStatus] -> StationInformation -> Bool
elemInStatusList ss si = any (\x -> SS.station_id x == SI.station_id si) ss
