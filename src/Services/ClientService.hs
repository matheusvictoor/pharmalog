{-# LANGUAGE BangPatterns #-}
module Services.ClientService (createClient, getAllClients, getClientByCpf, updateClient, deleteClient, addSaleToClient) where

import Models.Client
import Models.Sale (Sale)  -- Certifique-se de importar o tipo `Sale` corretamente
import Data.List (find, deleteBy)

createClient :: IO ()
createClient = do
  client <- Client
    <$> (putStrLn "Nome: " >> getLine)
    <*> (putStrLn "Idade: " >> getLine >>= return . read)
    <*> (putStrLn "Endereço: " >> getLine)
    <*> (putStrLn "CPF: " >> getLine)
    <*> (putStrLn "Telefone: " >> getLine)
    <*> return []  -- Inicializa a lista de vendas vazia
  appendFile "_customerDB.dat" (show client ++ "\n")
  putStrLn "** Cliente cadastrado com sucesso! **"

getAllClients :: IO [Client]
getAllClients = do
  contents <- readFile "_customerDB.dat"
  return $ map read (lines contents)

getClientByCpf :: String -> IO (Maybe Client)
getClientByCpf searchCpf = do
  clients <- getAllClients
  return $ find (\client -> cpf client == searchCpf) clients

updateClient :: String -> IO ()
updateClient searchCpf = do
  clients <- getAllClients
  let updatedClients = map updateIfFound clients
  writeFile "_customerDB.dat" (unlines $ map show updatedClients)
  putStrLn "** Cliente atualizado com sucesso! **"
  where
    updateIfFound client
      | cpf client == searchCpf = client { 
                                      name = "Novo nome",  
                                      age = 30,            
                                      address = "Novo endereço", 
                                      phone = "Novo telefone" 
                                    }
      | otherwise = client

deleteClient :: String -> IO ()
deleteClient searchCpf = do
  clients <- getAllClients
  let filteredClients = filter (\client -> cpf client /= searchCpf) clients
  writeFile "_customerDB.dat" (unlines $ map show filteredClients)
  putStrLn "** Cliente deletado com sucesso! **"

addSaleToClient :: String -> Sale -> IO ()
addSaleToClient searchCpf newSale = do
  clients <- getAllClients
  let updatedClients = map addSale clients
  writeFile "_customerDB.dat" (unlines $ map show updatedClients)
  putStrLn "** Venda adicionada ao cliente com sucesso! **"
  where
    addSale client
      | cpf client == searchCpf = client { sales = newSale : sales client }
      | otherwise = client
