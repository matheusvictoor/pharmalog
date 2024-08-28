module Models.Manager where

data Manager = Manager
  { gerenteId   :: Int
  , nome        :: String
  , email       :: String
  , telefone    :: String
  } deriving (Show, Read, Eq)
