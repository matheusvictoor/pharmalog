{-# LANGUAGE BangPatterns #-}
module Services.ProductService 
  ( createProduct
  , getProductById
  , deleteProduct
  , updateProduct
  , getAllProducts
  , alertLowStockProducts
  , alertExpiringProducts
  ) where

import Models.Product
import Data.List (find)
import Data.Time.Clock()
import Data.Time
    ( UTCTime,
      addUTCTime,
      getCurrentTime,
      defaultTimeLocale,
      parseTimeM )

data Index a = Index { index :: Int, productData :: a } deriving (Show, Read)

-- Função para criar um novo produto
createProduct :: IO ()
createProduct = do
  -- Solicitar informações do novo produto ao usuário
  newProduct <- Product
    <$> (putStrLn "Nome: " >> getLine)
    <*> (putStrLn "Descrição: " >> getLine)
    <*> (putStrLn "Categoria: " >> getLine)
    <*> (putStrLn "Data de Fabricação (YYYY-MM-DD): " >> getLine >>= parseDate)
    <*> (putStrLn "Data de Expiração (YYYY-MM-DD): " >> getLine >>= parseDate)
    <*> (putStrLn "Preço: " >> readLn)
    <*> (putStrLn "Estoque: " >> readLn)
  
  -- Salvar o novo produto no arquivo _productDB.dat
  appendFile "_productDB.dat" (show newProduct ++ "\n")
  putStrLn "** Produto cadastrado com sucesso! **"

-- A função parseDate, que faz a conversão da string para a data:
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
      let prod = productData (read line :: Index Product)  
      in if name prod == searchName
         then show (Index (index (read line :: Index Product)) (Product
            { name = searchName
            , description = "Nova descrição"
            , category = "Nova categoria"
            , dateManufacture = dateManufacture prod
            , expirationDate = expirationDate prod
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
  mapM_ alertProductStock products
  where
    alertProductStock prod =
      if stock prod < limit
        then putStrLn $ "Alerta! Produto: " ++ name prod ++ " está com estoque baixo. Estoque atual: " ++ show (stock prod)
        else putStrLn $ "Produto: " ++ name prod ++ " está com estoque suficiente. Estoque atual: " ++ show (stock prod)



alertExpiringProducts :: Int -> IO ()
alertExpiringProducts daysBefore = do
  products <- getAllProducts
  currentTime <- getCurrentTime
  mapM_ (checkProductExpiration currentTime daysBefore) products

checkProductExpiration :: UTCTime -> Int -> Product -> IO ()
checkProductExpiration currentTime daysBefore prod = do
  let expiringDate = addUTCTime (fromIntegral (daysBefore * 86400)) currentTime
  if expirationDate prod <= expiringDate
    then putStrLn $ "Alerta! O produto \"" ++ name prod ++ "\" está perto de vencer ou já venceu. Data de Expiração: " ++ show (expirationDate prod)
    else putStrLn $ "O produto \"" ++ name prod ++ "\" está ok. Data de Expiração: " ++ show (expirationDate prod)