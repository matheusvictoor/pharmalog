{-# LANGUAGE BangPatterns #-}
module Services.ProductService (createProduct, getProductById, deleteProduct, updateProduct, getAllProducts, alertLowStockProducts) where

import Models.Product
import Data.List (find, deleteBy)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Clock (UTCTime)

data Index a = Index { index :: Int, productData :: a } deriving (Show, Read)

createProduct :: IO ()
createProduct = do
  !productId <- fmap (length . lines) (readFile "_productDB.dat")
  
  product <- Product
    <$> (putStrLn "Nome: " >> getLine)
    <*> (putStrLn "Descrição: " >> getLine)
    <*> (putStrLn "Categoria: " >> getLine)
    
    
    <*> (putStrLn "Data de Fabricação (YYYY-MM-DD): " >> getLine >>= parseDate)
    
    <*> (putStrLn "Data de Expiração (YYYY-MM-DD): " >> getLine >>= parseDate)
    
    <*> (putStrLn "Preço: " >> readLn)
    <*> (putStrLn "Estoque: " >> readLn)
  
  appendFile "_productDB.dat" (show (Index (1+productId) product) ++ "\n")
  putStrLn "** Produto cadastrado com sucesso! **"


parseDate :: String -> IO UTCTime
parseDate str =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" str of
    Just date -> return date
    Nothing   -> fail "Formato de data inválido. Use o formato YYYY-MM-DD."


getProductById :: String -> IO (Maybe Product)
getProductById searchName = do
  contents <- readFile "_productDB.dat"
  let products = map (productData . read) (lines contents) :: [Product]
  return $ find (\p -> name p == searchName) products

deleteProduct :: String -> IO ()
deleteProduct searchName = do
  contents <- readFile "_productDB.dat"
  let products = lines contents
  let filteredProducts = filter (\line -> name (productData (read line :: Index Product)) /= searchName) products
  writeFile "_productDB.dat" (unlines filteredProducts)
  putStrLn "** Produto deletado com sucesso! **"

updateProduct :: String -> IO ()
updateProduct searchName = do
  contents <- readFile "_productDB.dat"
  let products = lines contents
  let updatedProducts = map updateIfFound products
  writeFile "_productDB.dat" (unlines updatedProducts)
  putStrLn "Produto atualizado com sucesso! "
  where
    updateIfFound line =
      let product = productData (read line :: Index Product)
      in if name product == searchName
         then show (Index (index (read line :: Index Product)) (Product
            { name = searchName
            , description = "Nova descrição"
            , category = "Nova categoria"
            , dateManufacture = dateManufacture product
            , expirationDate = expirationDate product
            , price = 19.99
            , stock = 150
            }))
         else line

getAllProducts :: IO [Product]
getAllProducts = do
  contents <- readFile "_productDB.dat"
  return $ map (productData . read) (lines contents)

alertLowStockProducts :: Int -> IO ()
alertLowStockProducts limit = do
  products <- getAllProducts
  let lowStockProducts = filter (\p -> stock p < limit) products
  if null lowStockProducts
    then putStrLn "Todos os produtos estão com estoque suficiente."
    else do
      putStrLn "Alerta! Produtos com estoque baixo:"
      mapM_ (\p -> putStrLn $ "Produto: " ++ name p ++ ", Estoque: " ++ show (stock p)) lowStockProducts
