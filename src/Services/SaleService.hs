{-# LANGUAGE BangPatterns #-}
module Services.SaleService (createSale, getAllSales, getSaleByClientId, updateSale, deleteSale) where

import Models.Sale
import Data.List (find, deleteBy)
import Data.Time.Clock (UTCTime)

data Index a = Index { index :: Int, saleData :: a } deriving (Show, Read)

createSale :: IO ()
createSale = do
  !saleId <- fmap (length . lines) (readFile "_SaleDB.dat")
  sale <- Sale
    <$> (putStrLn "ID do Cliente: " >> readLn)
    <*> (putStrLn "ID do Vendedor: " >> readLn)
    <*> (putStrLn "Valor da Venda: " >> readLn)
    <*> (putStrLn "Data da Venda (YYYY-MM-DD): " >> getLine >>= parseDate)
    <*> return []
  appendFile "_SaleDB.dat" (show (Index (1+saleId) sale) ++ "\n")
  putStrLn "** Venda registrada com sucesso! **"

getAllSales :: IO [Sale]
getAllSales = do
  contents <- readFile "_SaleDB.dat"
  return $ map (saleData . read) (lines contents)

getSaleByClientId :: Int -> IO (Maybe Sale)
getSaleByClientId searchClientId = do
  sales <- getAllSales
  return $ find (\sale -> clientId sale == searchClientId) sales

updateSale :: Int -> IO ()
updateSale searchClientId = do
  contents <- readFile "_SaleDB.dat"
  let sales = lines contents
  let updatedSales = map updateIfFound sales
  writeFile "_SaleDB.dat" (unlines updatedSales)
  putStrLn "** Venda atualizada com sucesso! **"
  where
    updateIfFound line =
      let sale = saleData (read line :: Index Sale)
      in if clientId sale == searchClientId
         then show (Index (index (read line :: Index Sale)) (Sale
            { clientId = searchClientId
            , sellerId = sellerId sale
            , totalSale = totalSale sale
            , dateSale = dateSale sale
            , products = products sale
            }))
         else line

deleteSale :: Int -> IO ()
deleteSale searchClientId = do
  contents <- readFile "_SaleDB.dat"
  let sales = lines contents
  let filteredSales = filter (\line -> clientId (saleData (read line :: Index Sale)) /= searchClientId) sales
  writeFile "_SaleDB.dat" (unlines filteredSales)
  putStrLn "** Venda deletada com sucesso! **"

-
parseDate :: String -> IO UTCTime
parseDate str =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" str of
    Just date -> return date
    Nothing   -> fail "Formato de data inválido. Use o formato YYYY-MM-DD."
