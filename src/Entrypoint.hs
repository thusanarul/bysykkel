{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Entrypoint (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Char8 as S8
import Data.Maybe (fromJust, isJust, isNothing)
import GHC.Generics
import Network.HTTP.Simple (getResponseBody, httpJSON)
import Network.Wai.Handler.Warp
import Servant
import StationInformation (StationInformation, getClosestStations, station_id)
import StationStatus (StationStatus, findAvailabilityForStations)
import UserPos (UserPos (UserPos, lat))

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

-- /available_stations?lat={lat}&lon={lon}&count={count} -- lat/lon is non-optional
type BysykkelAPI =
  "available_stations"
    :> QueryParam "lat" Float
    :> QueryParam "lon" Float
    :> QueryParam "count" Int
    :> Get '[JSON] [StationStatus]
    :<|> "health"
      :> Get '[PlainText] String

getStationStatus :: IO [StationStatus]
getStationStatus = do
  response <- httpJSON "https://gbfs.urbansharing.com/oslobysykkel.no/station_status.json"
  return $ (stations . model_data) (getResponseBody response :: Model StationStatus)

handleAvailableStations :: [StationInformation] -> Maybe Float -> Maybe Float -> Maybe Int -> Handler [StationStatus]
handleAvailableStations station_information lat lon count =
  if isJust lat && isJust lon
    then do
      station_statuses <- liftIO getStationStatus
      let pos = UserPos (fromJust lat) (fromJust lon)
      let closest = map station_id $ getClosestStations pos station_information count
       in return $ findAvailabilityForStations station_statuses closest
    else throwError err400 {errBody = "Missing lat and/or lon query parameters"}

handleHealth :: Handler String
handleHealth = return "OK"

server :: [StationInformation] -> Server BysykkelAPI
server stations = handleAvailableStations stations :<|> handleHealth

main :: IO ()
main = do
  putStrLn "Bysykkel API!"
  response <- httpJSON "https://gbfs.urbansharing.com/oslobysykkel.no/station_information.json"
  let station_information = (stations . model_data) (getResponseBody response :: Model StationInformation)
  -- this does not have to be one-liner, but it's rad yeah
  run 8000 $ serve (Proxy :: Proxy BysykkelAPI) (server station_information)
