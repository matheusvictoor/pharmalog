module Models.Seller where

data Vendedor = Vendedor
  { vendedorId :: Int
  , nome       :: String
  , email      :: String
  , telefone   :: String
  } deriving (Show, Eq, Read)
