{-# LANGUAGE BangPatterns #-}
module Services.SaleService (createSale, getAllSales, getSaleByClientCpf, updateSale, deleteSale, menuSale) where

import Models.Sale
import Data.List (find)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import System.IO (hFlush, stdout)

data Index a = Index { index :: Int, saleData :: a } deriving (Show, Read)

createSale :: IO ()
createSale = do
  !saleId <- fmap (length . lines) (readFile "_saleDB.dat")
  sale <- Sale
    <$> (putStrLn "CPF do Cliente: " >> readLn)
    <*> (putStrLn "ID do Vendedor: " >> readLn)
    <*> (putStrLn "Data da Venda (YYYY-MM-DD): " >> getLine >>= parseDate)
    <*> (putStrLn "Valor da Venda (9.99): " >> readLn)
    <*> return []
  appendFile "_saleDB.dat" (show (Index (1 + saleId) sale) ++ "\n")
  putStrLn "** Venda registrada com sucesso! **"

getAllSales :: IO ()
getAllSales = do
  contents <- readFile "_saleDB.dat"
  let sales = map (read :: String -> Index Sale) (lines contents)
  mapM_ printSale sales

getSaleByClientCpf :: IO ()
getSaleByClientCpf = do
  putStrLn "CPF do Cliente: "
  searchClientCpf <- readLn
  contents <- readFile "_saleDB.dat"
  let sales = map (read :: String -> Index Sale) (lines contents)
  case find (\(Index _ sale) -> clientId sale == searchClientCpf) sales of
    Just s  -> printSale s
    Nothing -> putStrLn "Venda não encontrada."

updateSale :: Int -> IO ()
updateSale searchClientId = do
  contents <- readFile "_saleDB.dat"
  let sales = lines contents
  let updatedSales = map updateIfFound sales
  writeFile "_saleDB.dat" (unlines updatedSales)
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
  contents <- readFile "_saleDB.dat"
  let sales = lines contents
  let filteredSales = filter (\line -> clientId (saleData (read line :: Index Sale)) /= searchClientId) sales
  writeFile "_saleDB.dat" (unlines filteredSales)
  putStrLn "** Venda deletada com sucesso! **"

parseDate :: String -> IO UTCTime
parseDate str =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" str of
    Just date -> return date
    Nothing   -> fail "Formato de data inválido. Use o formato YYYY-MM-DD."

printSale :: Index Sale -> IO ()
printSale (Index idx sale) = do
  putStrLn $ "ID da Venda: " ++ show idx
  putStrLn $ "CPF do Cliente: " ++ show (clientId sale)
  putStrLn $ "ID do Vendedor: " ++ show (sellerId sale)
  putStrLn $ "Data da Venda: " ++ show (dateSale sale)
  putStrLn $ "Valor Total: " ++ show (totalSale sale)
  putStrLn $ "Produtos: " ++ show (products sale)
  putStrLn "----------------------------------------"

menuSale :: IO ()
menuSale = do
  putStrLn "\nSelecione uma opção:"
  putStrLn "1. Cadastrar um nova venda"
  putStrLn "2. Buscar um cliente por CPF"
  putStrLn "3. Buscar todas as vendas"
  putStrLn "0 <- Voltar"

  putStr "\nOpção -> "
  hFlush stdout

  option <- getLine

  case option of
    "1" -> createSale
    "2" -> getSaleByClientCpf
    "3" -> getAllSales
    "0" -> putStrLn "\n<---"
    _   -> putStrLn "Opção inválida. Tente novamente." >> menuSale

  putStrLn ""
