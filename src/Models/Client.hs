module Models.Client where

import Models.Sale (Sale)

data Client = Client
  { name    :: String
  , age     :: Int
  , address :: String
  , cpf     :: String
  , phone   :: String
  , sales   :: [Sale]
  } deriving (Show, Read, Eq)
