{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Entrypoint (main) where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Simple (getResponseBody, httpJSON)
import StationInformation (StationInformation)
import StationStatus (StationStatus)

apiBasePath :: String
apiBasePath = "https://gbfs.urbansharing.com/oslobysykkel.no/"

data Data s = Data
  {stations :: [s]}
  deriving (Show)

instance (FromJSON s) => FromJSON (Data s) where
  parseJSON (Object v) = do
    stations <- v .: "stations"
    pure $ Data {..}
  parseJSON invalid = typeMismatch "Data" invalid

data Model s = Model
  { last_updated :: Int,
    model_data :: Data s
  }
  deriving (Show)

instance (FromJSON d) => FromJSON (Model d) where
  parseJSON (Object v) = do
    last_updated <- v .: "last_updated"
    model_data <- v .: "data"
    pure $ Model {..}
  parseJSON invalid = typeMismatch "Model" invalid

main :: IO ()
main = do
  putStrLn "Bysykkel API!"
  response <- httpJSON "https://gbfs.urbansharing.com/oslobysykkel.no/station_information.json"
  let station_information = (getResponseBody response :: Model StationInformation)
  print station_information
  response <- httpJSON "https://gbfs.urbansharing.com/oslobysykkel.no/station_status.json"
  let station_status = (getResponseBody response :: Model StationStatus)
  print station_status
