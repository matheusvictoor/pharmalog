{-# LANGUAGE BangPatterns #-}

module Services.ProductService ( createProduct, getProductById, deleteProduct, updateProduct, getAllProducts, alertLowStockProducts, alertExpiringProducts, menuProduct) where

import Models.Product
import Data.List (find)
import Data.Time.Clock()
import Data.Time ( UTCTime, addUTCTime, getCurrentTime, defaultTimeLocale, parseTimeM )
import System.IO (hFlush, stdout)

data Index a = Index { index :: Int, productData :: a } deriving (Show, Read)

-- Função para criar um novo produto
createProduct :: IO ()
createProduct = do
  !productId <- fmap (length . lines) (readFile "_productDB.dat")
  newProduct <- Product
    <$> (putStrLn "Nome: " >> getLine)
    <*> (putStrLn "Descrição: " >> getLine)
    <*> (putStrLn "Categoria: " >> getLine)
    <*> (putStrLn "Data de Fabricação (YYYY-MM-DD): " >> getLine >>= parseDate)
    <*> (putStrLn "Data de Expiração (YYYY-MM-DD): " >> getLine >>= parseDate)
    <*> (putStrLn "Preço (9.99): " >> readLn)
    <*> (putStrLn "Estoque: " >> readLn)
  
  -- Salvar o novo produto no arquivo _productDB.dat
  appendFile "_productDB.dat" (show (Index (1+productId) newProduct) ++ "\n")
  putStrLn "** Produto cadastrado com sucesso! **"

-- A função parseDate, que faz a conversão da string para a data:
parseDate :: String -> IO UTCTime
parseDate str =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" str of
    Just date -> return date
    Nothing   -> fail "Formato de data inválido. Use o formato YYYY-MM-DD."

getProductById :: IO ()
getProductById = do
  putStrLn "ID do produto para buscar: "
  productId <- readLn
  contents <- readFile "_productDB.dat"

  let products = map (read :: String -> Index Product) (lines contents)

  case find (\p -> index p == productId) products of
    Just p -> putStrLn $ "Informações do produto:\n" ++ show p
    Nothing -> putStrLn "Produto não encontrado."

getProductByName :: IO ()
getProductByName = do
  putStrLn "Nome do produto para buscar: "
  productNameSearched <- getLine
  contents <- readFile "_productDB.dat"

  let products = map (productData . read) (lines contents)

  case find (\p -> nameProduct p == productNameSearched) products of
    Just p -> putStrLn $ "Informações do produto:\n" ++ show p
    Nothing -> putStrLn "Produto não encontrado."

deleteProduct :: String -> IO ()
deleteProduct searchName = do
  contents <- readFile "_productDB.dat"
  let products = lines contents
  let filteredProducts = filter (\line -> nameProduct (productData (read line :: Index Product)) /= searchName) products
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
      in if nameProduct prod == searchName
         then show (Index (index (read line :: Index Product)) (Product
            { nameProduct = searchName
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
        then putStrLn $ "Alerta! Produto: " ++ nameProduct prod ++ " está com estoque baixo. Estoque atual: " ++ show (stock prod)
        else putStrLn $ "Produto: " ++ nameProduct prod ++ " está com estoque suficiente. Estoque atual: " ++ show (stock prod)

alertExpiringProducts :: Int -> IO ()
alertExpiringProducts daysBefore = do
  products <- getAllProducts
  currentTime <- getCurrentTime
  mapM_ (checkProductExpiration currentTime daysBefore) products

checkProductExpiration :: UTCTime -> Int -> Product -> IO ()
checkProductExpiration currentTime daysBefore prod = do
  let expiringDate = addUTCTime (fromIntegral (daysBefore * 86400)) currentTime
  if expirationDate prod <= expiringDate
    then putStrLn $ "Alerta! O produto \"" ++ nameProduct prod ++ "\" está perto de vencer ou já venceu. Data de Expiração: " ++ show (expirationDate prod)
    else putStrLn $ "O produto \"" ++ nameProduct prod ++ "\" está ok. Data de Expiração: " ++ show (expirationDate prod)
  
menuProduct :: IO ()
menuProduct = do
  putStrLn "Selecione uma opção:"
  putStrLn "1.  Cadastrar um novo produto"
  putStrLn "2.  Buscar um produto por ID"
  putStrLn "3.  Buscar um produto por name"
  putStrLn "4.  Buscar todos os produtos"
  putStrLn "5.  Atualizar um produto"
  putStrLn "6.  Remove um produto"
  putStrLn "7.  Alertar sobre Baixo Estoque"
  putStrLn "8.  Alertar sobre Produtos Perto de Vencer"
  putStrLn "0 <- Voltar"

  putStr "Opção -> "
  hFlush stdout

  option <- getLine
  
  case option of
    "1" -> createProduct
    "2" -> getProductById
    "3" -> getProductByName
    -- "4" -> getAllProducts
    -- "5" -> updateProduct
    -- "6" -> deleteProduct
    -- "7" -> alertLowStockProducts
    -- "8" -> alertExpiringProducts
    "0" -> putStrLn "<---"
    _ -> putStrLn "Opção inválida. Tente novamente." >> menuProduct
  putStrLn ""
