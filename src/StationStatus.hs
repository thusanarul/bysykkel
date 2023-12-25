module StationStatus where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.List (find)
import Data.Maybe (mapMaybe)
import StationInformation (StationInformation)

data StationStatus = StationStatus
  { is_installed :: Bool,
    is_renting :: Bool,
    num_bikes_available :: Int,
    num_docks_available :: Int,
    last_reported :: Int,
    is_returning :: Bool,
    station_id :: String
  }
  deriving (Show, Eq)

instance FromJSON StationStatus where
  parseJSON (Object v) = do
    is_installed <- v .: "is_installed"
    is_renting <- v .: "is_renting"
    num_bikes_available <- v .: "num_bikes_available"
    num_docks_available <- v .: "num_docks_available"
    last_reported <- v .: "last_reported"
    is_returning <- v .: "is_returning"
    station_id <- v .: "station_id"
    pure $ StationStatus {..}
  parseJSON invalid = typeMismatch "StationStatus" invalid

findAvailabilityForStation :: [StationStatus] -> String -> Maybe StationStatus
findAvailabilityForStation stations stationId =
  find (\s -> stationId == station_id s) stations

-- Function to find all station availability information from a list of station ids
findAvailabilityForStations :: [StationStatus] -> [String] -> [StationStatus]
findAvailabilityForStations stations = mapMaybe (findAvailabilityForStation stations)
