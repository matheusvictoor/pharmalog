module Models.Sale where

data Sale = Sale {
  clientId :: Int,
  dateSale :: UTCTime,
  totalSale :: Double,
  productsSale :: [Product]
} deriving (Show, Read, Eq)
