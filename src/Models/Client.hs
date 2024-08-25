module Models.Client where

data Client = Client { 
  name :: String,
  cpf :: String,
  address :: String,
  phone :: String
} deriving (Show, Read, Eq)