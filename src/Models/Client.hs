module Models.Client where

import Models.Sale (Sale)

data Client = Client { 
  nameClient :: String,
  age     :: Int,
  cpf :: String,
  address :: String,
  phone :: String,
  sales :: [Sale]
} deriving (Show, Read, Eq)
