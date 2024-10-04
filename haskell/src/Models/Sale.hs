module Models.Sale where
import Data.Time.Clock (UTCTime)
import Models.Product (Product)

data Sale = Sale {
  clientId :: Int,
  sellerId :: Int,
  dateSale :: UTCTime,
  totalSale :: Double,
  products :: [Product]
} deriving (Show, Read, Eq)
