module Models.Product where

import Data.Time.Clock (UTCTime)
data Product = Product
  { name            :: String
  , description     :: String
  , category        :: String
  , dateManufacture :: UTCTime
  , expirationDate  :: UTCTime
  , price           :: Float
  , stock           :: Int
  } deriving (Show, Read, Eq)
