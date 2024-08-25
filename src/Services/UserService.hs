{-# LANGUAGE BangPatterns #-}
module Services.UserService (createUser) where

import Models.User

data Index a = Index { index :: Int, userData :: a } deriving (Show, Read)

createUser :: IO ()
createUser = do
  !userId <- fmap (length . lines) (readFile "_userDB.dat")
  user <- User <$> (putStrLn "Nome: " >> getLine) <*> (putStrLn "Password: " >> getLine) <*> (putStrLn "Role (Administrador | Gerente | Vendedor): " >> readLn)
  appendFile "_userDB.dat" (show (Index (1+userId) user) ++ "\n")
  putStrLn "** Usuario cadastrado com sucesso! **"