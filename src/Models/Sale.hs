module Models.Sale where

data Sale = Sale {
  clientId :: Int,
  dateSale :: UTCTime,
  totalSale :: Double
} deriving (Show, Read, Eq)