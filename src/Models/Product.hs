module Models.Product where

data Product = {
  name :: String,
  description :: String,
  category :: String,
  dateManufacture :: UTCTime,
  expirationDate :: UTCTime,
  price :: Float,
  stock :: Int
} deriving (Show, Read, Eq)