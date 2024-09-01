module Models.Product where
import Data.Time.Clock (UTCTime)

data Product = Product{ 
  nameProduct :: String,
  description :: String,
  category :: String,
  dateManufacture :: UTCTime,
  expirationDate :: UTCTime,
  price :: Float,
  stock :: Int
} deriving (Show, Read, Eq)
