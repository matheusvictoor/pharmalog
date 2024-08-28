{-# LANGUAGE BangPatterns #-}
module Services.ClientService (createClient, getAllClients, getClientByCpf, updateClient, deleteClient, addSaleToClient) where

import Models.Client
import Models.Sale
import Data.List (find, deleteBy)

data Index a = Index { index :: Int, clientData :: a } deriving (Show, Read)

createClient :: IO ()
createClient = do
  !clientId <- fmap (length . lines) (readFile "_customerDB.dat")
  client <- Client
    <$> (putStrLn "Nome: " >> getLine)
    <*> (putStrLn "CPF: " >> getLine)
    <*> (putStrLn "Endereço: " >> getLine)
    <*> (putStrLn "Telefone: " >> getLine)
    <*> return []
  appendFile "_customerDB.dat" (show (Index (1+clientId) client) ++ "\n")
  putStrLn "** Cliente cadastrado com sucesso! **"

getAllClients :: IO [Client]
getAllClients = do
  contents <- readFile "_customerDB.dat"
  return $ map (clientData . read) (lines contents)

getClientByCpf :: String -> IO (Maybe Client)
getClientByCpf searchCpf = do
  clients <- getAllClients
  return $ find (\client -> cpf client == searchCpf) clients

updateClient :: String -> IO ()
updateClient searchCpf = do
  contents <- readFile "_customerDB.dat"
  let clients = lines contents
  let updatedClients = map updateIfFound clients
  writeFile "_customerDB.dat" (unlines updatedClients)
  putStrLn "** Cliente atualizado com sucesso! **"
  where
    updateIfFound line =
      let client = clientData (read line :: Index Client)
      in if cpf client == searchCpf
         then show (Index (index (read line :: Index Client)) (Client
            { name = "Novo nome"
            , cpf = searchCpf
            , address = "Novo endereço"
            , phone = "Novo telefone"
            , sales = sales client
            }))
         else line

deleteClient :: String -> IO ()
deleteClient searchCpf = do
  contents <- readFile "_customerDB.dat"
  let clients = lines contents
  let filteredClients = filter (\line -> cpf (clientData (read line :: Index Client)) /= searchCpf) clients
  writeFile "_customerDB.dat" (unlines filteredClients)
  putStrLn "** Cliente deletado com sucesso! **"

addSaleToClient :: String -> Sale -> IO ()
addSaleToClient searchCpf newSale = do
  contents <- readFile "_customerDB.dat"
  let clients = lines contents
  let updatedClients = map updateIfFound clients
  writeFile "_customerDB.dat" (unlines updatedClients)
  putStrLn "** Venda adicionada ao cliente com sucesso! **"
  where
    updateIfFound line =
      let client = clientData (read line :: Index Client)
      in if cpf client == searchCpf
         then show (Index (index (read line :: Index Client)) (client { sales = newSale : sales client }))
         else line
