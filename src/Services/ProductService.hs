module Services.ProductService ( createProduct,  ) where

import Models.Product

import Data.List (find, deleteBy)

createProduct :: Product -> [Product] -> [Product]
createProduct newProduct products = newProduct : products

getProductById :: String -> [Product] -> Maybe Product
getProductById searchName = find (\p -> name p == searchName)

deleteProduct :: String -> [Product] -> [Product]
deleteProduct searchName = deleteBy (\p _ -> name p == searchName) undefined

updateProduct :: String -> Product -> [Product] -> [Product]
updateProduct searchName updatedProduct = map updateIfFound
  where
    updateIfFound product
      | name product == searchName = updatedProduct
      | otherwise = product

getAllProducts :: [Product] -> [Product]
getAllProducts = id

alertLowStockProducts :: Int -> [Product] -> IO ()
alertLowStockProducts limit products = do
  let lowStockProducts = filter (\p -> stock p < limit) products
  if null lowStockProducts
    then putStrLn "Todos os produtos estão com estoque suficiente."
    else do
      putStrLn "Alerta! Produtos com estoque baixo:"
      mapM_ (\p -> putStrLn $ "Produto: " ++ name p ++ ", Estoque: " ++ show (stock p)) lowStockProducts