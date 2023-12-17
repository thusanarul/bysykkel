module LatLon where

class LatLon a where
  getLat :: a -> Float
  getLon :: a -> Float
