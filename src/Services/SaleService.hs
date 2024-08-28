module Services.SaleService where

import Models.Sale
import Data.List (find, deleteBy)
import Data.Time.Clock (UTCTime)

createSale :: Sale -> [Sale] -> [Sale]
createSale newSale sales = newSale : sales

getAllSales :: [Sale] -> [Sale]
getAllSales = id

getSaleByClientId :: Int -> [Sale] -> Maybe Sale
getSaleByClientId searchClientId = find (\sale -> clientId sale == searchClientId)

updateSale :: Int -> Sale -> [Sale] -> [Sale]
updateSale searchClientId updatedSale = map updateIfFound
  where
    updateIfFound sale
      | clientId sale == searchClientId = updatedSale
      | otherwise = sale

deleteSale :: Int -> [Sale] -> [Sale]
deleteSale searchClientId = deleteBy (\sale _ -> clientId sale == searchClientId) undefined
