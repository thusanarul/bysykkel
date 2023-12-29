{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module StationInformation (calculateDistance, getClosestStations, StationInformation (..)) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Function ((&))
import Data.List (sort, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Ord as Ordering
import GHC.Generics
import LatLon (LatLon (..))

radianConstant = pi / 180.0

rConstant = 6371e3

data StationInformation = StationInformation
  { station_id :: String,
    name :: String,
    address :: String,
    lat :: Float,
    lon :: Float
  }
  deriving (Show, Generic)

instance ToJSON StationInformation

instance FromJSON StationInformation where
  parseJSON (Object v) = do
    station_id <- v .: "station_id"
    name <- v .: "name"
    address <- v .: "address"
    lat <- v .: "lat"
    lon <- v .: "lon"
    pure $ StationInformation {..}
  parseJSON invalid = typeMismatch "StationInformation" invalid

instance LatLon StationInformation where
  getLat a = lat (a :: StationInformation)
  getLon :: StationInformation -> Float
  getLon a = lon (a :: StationInformation)

data StationDistance = StationDistance {distance :: Float, station :: StationInformation}

instance Eq StationDistance where
  (==) :: StationDistance -> StationDistance -> Bool
  (==) (StationDistance a _) (StationDistance b _) = a == b

instance Ord StationDistance where
  compare :: StationDistance -> StationDistance -> Ordering
  compare (StationDistance a _) (StationDistance b _) = compare a b

getClosestStations :: (LatLon a) => a -> [StationInformation] -> Maybe Int -> [StationInformation]
getClosestStations pos stations n =
  take (fromMaybe 3 n) . map station . sort $ map (distanceForStation pos) stations

distanceForStation :: (LatLon a) => a -> StationInformation -> StationDistance
distanceForStation pos station =
  let distance = calculateDistance pos station
   in StationDistance {distance = distance, station = station}

-- Uses haversine formula: https://www.movable-type.co.uk/scripts/latlong.html
calculateDistance :: (LatLon a) => (LatLon b) => a -> b -> Float
calculateDistance pos1 pos2 =
  let phi_1 = getLat pos1 * radianConstant
      phi_2 = getLat pos2 * radianConstant
      thetha_phi = (getLat pos2 - getLat pos1) * radianConstant
      thetha_lambda = (getLon pos2 - getLon pos1) * radianConstant
      a = sin (thetha_phi / 2.0) ** 2 + cos phi_1 * cos phi_2 * sin (thetha_lambda / 2.0) ** 2
      c = 2.0 * atan2 (sqrt a) (sqrt (1.0 - a))
   in rConstant * c
