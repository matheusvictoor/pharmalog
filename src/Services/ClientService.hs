import Data.List (intercalate)

n_salesClient :: Client -> Int
n_salesClient cliente = length (sales cliente)

levelClient :: Client -> String
levelClient cliente
   |(n_salesClient cliente) >= 50 = “Cliente nível ouro!”
   |(n_salesClient cliente) >= 25 = “Cliente nível prata!”
   |otherwise = “Cliente nível bronze!”

relatoryProduct :: Product -> String
relatoryProduct (Product name description category dateManufacture expirationDate price stock) = name ++ ": Descrição = " ++ show description ++ ", Categoria = " ++ show category ++ ", Data de Produção = " ++ show dateManufacture ++ ", Data de Validade = " ++ show expirationDate ++ ", Preço = " ++ show price ++ ", Estoque = " ++ show stock

relatoryProductClient :: Client -> String
relatoryProductClient cliente = intercalate "\n" (map relatoryProduct produtos)
   where produtos = concatMap producstSale (sales cliente)

