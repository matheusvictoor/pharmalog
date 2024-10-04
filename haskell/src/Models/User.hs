module Models.User where

data Role = Administrador | Gerente | Vendedor deriving (Read, Show)

data User = User { 
  name :: String,
  cpf :: String,
  password :: String,
  role :: Role
} deriving (Read, Show)