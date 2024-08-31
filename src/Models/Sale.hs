module Models.Sale where
import Data.Time.Clock (UTCTime)
import Models.Product (Product)

data Sale = Sale
  { clientId   :: Int
  , sellerId   :: Int      
  , totalSale  :: Double
  , dateSale   :: UTCTime
  , products   :: [Product] 
  } deriving (Show, Read, Eq)
