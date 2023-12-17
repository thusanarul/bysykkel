module UserPos (UserPos (..)) where

import LatLon (LatLon (..))

data UserPos = UserPos
  { lat :: Float,
    lon :: Float
  }

instance LatLon UserPos where
  getLat a = lat (a :: UserPos)
  getLon a = lon (a :: UserPos)
