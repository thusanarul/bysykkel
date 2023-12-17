{-# LANGUAGE RecordWildCards #-}

module StationStatus where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)

data StationStatus = StationStatus
  { is_installed :: Bool,
    is_renting :: Bool,
    num_bikes_available :: Int,
    num_docks_available :: Int,
    last_reported :: Int,
    is_returning :: Bool,
    station_id :: String
  }
  deriving (Show)

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
