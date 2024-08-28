module Services.SellerService where

import Models.Seller
import Data.List (find, deleteBy)

createVendedor :: Vendedor -> [Vendedor] -> [Vendedor]
createVendedor newVendedor vendedores = newVendedor : vendedores

getAllVendedores :: [Vendedor] -> [Vendedor]
getAllVendedores = id

getVendedorById :: Int -> [Vendedor] -> Maybe Vendedor
getVendedorById searchId = find (\vendedor -> vendedorId vendedor == searchId)

updateVendedor :: Int -> Vendedor -> [Vendedor] -> [Vendedor]
updateVendedor searchId updatedVendedor = map updateIfFound
  where
    updateIfFound vendedor
      | vendedorId vendedor == searchId = updatedVendedor
      | otherwise = vendedor

deleteVendedor :: Int -> [Vendedor] -> [Vendedor]
deleteVendedor searchId = deleteBy (\vendedor _ -> vendedorId vendedor == searchId) undefined
