module Models.Client where

data Client = Client { 
  name :: String,
  cpf :: String,
  address :: String,
  phone :: String
   , sales   :: [Sale]
} deriving (Show, Read, Eq)