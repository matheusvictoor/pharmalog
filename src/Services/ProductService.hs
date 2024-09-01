{-# LANGUAGE BangPatterns #-}

module Services.ProductService ( createProduct, getProductById, deleteProduct, updateProduct, getAllProducts, showAllProducts, alertLowStockProducts, alertExpiringProducts, menuProduct) where

import Models.Product
import Data.List (find, isPrefixOf)
import Data.Time.Clock()
import Data.Time ( UTCTime, addUTCTime, getCurrentTime, defaultTimeLocale, parseTimeM )
import System.IO (hFlush, stdout, readFile, writeFile, openTempFile, hClose, hGetContents, hPutStr)
import System.Directory (renameFile, removeFile)

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
    Just p -> do
      putStrLn $ "\nInformações do produto:\n"
      printProductWithIndex p
    Nothing -> putStrLn "Produto não encontrado."

getProductByName :: IO ()
getProductByName = do
  putStrLn "Nome do produto para buscar: "
  productNameSearched <- getLine
  contents <- readFile "_productDB.dat"
  let products = map (read :: String -> Index Product) (lines contents)
  case find (\p -> nameProduct (productData p) == productNameSearched) products of
    Just p -> do
      putStrLn $ "\nInformações do produto:\n"
      printProductWithIndex p
    Nothing -> putStrLn "Produto não encontrado."

deleteProduct :: IO ()
deleteProduct = do
  putStrLn "ID do produto a ser deletado: "
  productId <- getLine
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- readFile "_productDB.dat"
  let products = lines contents
      filteredProducts = filter (not . isPrefixOf ("Index {index = " ++ productId ++ ",") ) products
  hPutStr tempHandle (unlines filteredProducts)
  hClose tempHandle
  removeFile "_productDB.dat"
  renameFile tempName "_productDB.dat"
  putStrLn "** Produto deletado com sucesso! **"

updateProduct :: IO ()
updateProduct = do
  contents <- readFile "_productDB.dat"
  putStrLn "ID do produto para atualizar: "
  productId <- readLn
  let products = lines contents
  updatedProducts <- mapM (updateIfFound productId) products
  writeFile "_productDB.dat" (unlines updatedProducts)
  putStrLn "Produto atualizado com sucesso! "
  where
    updateIfFound :: Int -> String -> IO String
    updateIfFound productId line =
      let prod = productData (read line :: Index Product)
          prodIndex = index (read line :: Index Product)
      in if prodIndex == productId
        then do
          putStrLn "Informe os novos dados do produto:"
          putStrLn "Nome:"
          newName <- getLine

          putStrLn "Descrição:"
          newDescription <- getLine

          putStrLn "Categoria:"
          newCategory <- getLine

          putStrLn "Data de Fabricação (YYYY-MM-DD):"
          newManufactureDate <- getLine >>= parseDate

          putStrLn "Data de Expiração (YYYY-MM-DD):"
          newExpirationDate <- getLine >>= parseDate

          putStrLn "Preço:"
          newPrice <- readLn

          putStrLn "Estoque:"
          newStock <- readLn

          let updatedProduct = Product
                { nameProduct = newName
                , description = newDescription
                , category = newCategory
                , dateManufacture = newManufactureDate
                , expirationDate = newExpirationDate
                , price = newPrice
                , stock = newStock
                }

          putStrLn "\nProduto atualizado:"
          printProductWithIndex (Index prodIndex updatedProduct)

          return (show (Index prodIndex updatedProduct))
        else return line

getAllProducts :: IO [Product]
getAllProducts = do
  contents <- readFile "_productDB.dat"
  return $ map (productData . read) (lines contents)

showAllProducts :: IO ()
showAllProducts = do
  contents <- readFile "_productDB.dat"
  let products = map (read :: String -> Index Product) (lines contents)
  putStrLn "\nTodos os produtos cadastrados no sistema\n"
  mapM_ printProductWithIndex products


alertLowStockProducts :: IO ()
alertLowStockProducts = do
  putStrLn "Digite o limite de estoque para alerta: "
  limit <- readLn
  products <- getAllProducts
  mapM_ (alertProductStock limit) products
  where
    alertProductStock :: Int -> Product -> IO ()
    alertProductStock limit prod =
      if stock prod < limit
        then putStrLn $ "Alerta! Produto: " ++ nameProduct prod ++ " está com estoque baixo. Estoque atual: " ++ show (stock prod)
        else putStrLn $ "Produto: " ++ nameProduct prod ++ " está com estoque suficiente. Estoque atual: " ++ show (stock prod)

alertExpiringProducts :: IO ()
alertExpiringProducts = do
  putStrLn "Digite a quantidade de dias para alerta de vencimento: "
  daysBefore <- readLn
  products <- getAllProducts
  currentTime <- getCurrentTime
  mapM_ (checkProductExpiration currentTime daysBefore) products

checkProductExpiration :: UTCTime -> Int -> Product -> IO ()
checkProductExpiration currentTime daysBefore prod = do
  let expiringDate = addUTCTime (fromIntegral (daysBefore * 86400)) currentTime
  if expirationDate prod <= expiringDate
    then putStrLn $ "Alerta! O produto \"" ++ nameProduct prod ++ "\" está perto de vencer ou já venceu. Data de Expiração: " ++ show (expirationDate prod)
    else putStrLn $ "O produto \"" ++ nameProduct prod ++ "\" está ok. Data de Expiração: " ++ show (expirationDate prod)

--modelo de prinst para mostrar os produtos em tela
printProductWithIndex :: Index Product -> IO ()
printProductWithIndex (Index idx prod) = do
  putStrLn $ "ID: " ++ show idx
  putStrLn $ "Nome: " ++ nameProduct prod
  putStrLn $ "Descrição: " ++ description prod
  putStrLn $ "Categoria: " ++ category prod
  putStrLn $ "Data de Fabricação: " ++ show (dateManufacture prod)
  putStrLn $ "Data de Expiração: " ++ show (expirationDate prod)
  putStrLn $ "Preço: " ++ show (price prod)
  putStrLn $ "Estoque: " ++ show (stock prod)
  putStrLn "----------------------------------------"
 
menuProduct :: IO ()
menuProduct = do
  putStrLn "\nSelecione uma opção:"
  putStrLn "1.  Cadastrar um novo produto"
  putStrLn "2.  Buscar um produto por ID"
  putStrLn "3.  Buscar um produto por name"
  putStrLn "4.  Buscar todos os produtos"
  putStrLn "5.  Atualizar um produto"
  putStrLn "6.  Remove um produto"
  putStrLn "7.  Alertar sobre Baixo Estoque"
  putStrLn "8.  Alertar sobre Produtos Perto de Vencer"
  putStrLn "0 <- Voltar"

  putStr "\nOpção -> "
  hFlush stdout

  option <- getLine
  
  case option of
    "1" -> createProduct
    "2" -> getProductById
    "3" -> getProductByName
    "4" -> showAllProducts
    "5" -> updateProduct
    "6" -> deleteProduct
    "7" -> alertLowStockProducts
    "8" -> alertExpiringProducts
    "0" -> putStrLn "\n<---"
    _ -> putStrLn "Opção inválida. Tente novamente." >> menuProduct
  putStrLn ""
