{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Entrypoint (main) where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Char8 as S8
import Data.Maybe (isNothing)
import GHC.Generics
import Network.HTTP.Simple (getResponseBody, httpJSON)
import Network.Wai.Handler.Warp
import Servant
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

-- /available_stations?lat={lat}&lon={lon}&count={count} -- lat/lon is non-optional
type BysykkelAPI =
  "available_stations"
    :> QueryParam "lat" Float
    :> QueryParam "lon" Float
    :> QueryParam "count" Int
    :> Get '[PlainText] String
    :<|> "health"
      :> Get '[PlainText] String

handleAvailableStations :: Maybe Float -> Maybe Float -> Maybe Int -> Handler String
handleAvailableStations lat lon count =
  if isNothing lat || isNothing lon
    then throwError err400 {errBody = "Missing lat and/or lon query parameters"}
    else return "HEI"

handleHealth :: Handler String
handleHealth = return "OK"

server :: Server BysykkelAPI
server = handleAvailableStations :<|> handleHealth

main :: IO ()
main = do
  putStrLn "Bysykkel API!"
  response <- httpJSON "https://gbfs.urbansharing.com/oslobysykkel.no/station_information.json"
  let station_information = (getResponseBody response :: Model StationInformation)
  response <- httpJSON "https://gbfs.urbansharing.com/oslobysykkel.no/station_status.json"
  let station_status = (getResponseBody response :: Model StationStatus)
  -- this does not have to be one-liner, but it's rad yeah
  run 8000 $ serve (Proxy :: Proxy BysykkelAPI) server
