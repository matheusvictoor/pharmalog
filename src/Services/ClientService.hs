{-# LANGUAGE BangPatterns #-}
module Services.ClientService (
    createClient,
    getAllClients,
    getClientByCpf,
    updateClient,
    deleteClient,
    addSaleToClient,
    viewClientInfo
) where

import Models.Client
import Models.Product
import Models.Sale
import Data.List (find)
import Data.List (intercalate)

-- Função para criar um novo cliente
createClient :: IO ()
createClient = do
  client <- Client
    <$> (putStrLn "Nome: " >> getLine)
    <*> (putStrLn "Idade: " >> getLine >>= return . read)
    <*> (putStrLn "Endereço: " >> getLine)
    <*> (putStrLn "CPF: " >> getLine)
    <*> (putStrLn "Telefone: " >> getLine)
    <*> return []  
  appendFile "_customerDB.dat" (show client ++ "\n")
  putStrLn "** Cliente cadastrado com sucesso! **"

-- Função para obter todos os clientes
getAllClients :: IO [Client]
getAllClients = do
  contents <- readFile "_customerDB.dat"
  return $ map read (lines contents)

-- Função para buscar um cliente pelo CPF
getClientByCpf :: String -> IO (Maybe Client)
getClientByCpf searchCpf = do
  clients <- getAllClients
  return $ find (\client -> cpf client == searchCpf) clients

-- Função para visualizar as informações de um cliente
viewClientInfo :: String -> IO ()
viewClientInfo searchCpf = do
  client <- getClientByCpf searchCpf
  case client of
    Just c -> putStrLn $ "Informações do Cliente:\n" ++ show c
    Nothing -> putStrLn "Cliente não encontrado."

-- Função para atualizar as informações de um cliente
updateClient :: String -> IO ()
updateClient searchCpf = do
  clients <- getAllClients
  updatedClients <- mapM updateIfFound clients
  writeFile "_customerDB.dat" (unlines $ map show updatedClients)
  putStrLn "** Cliente atualizado com sucesso! **"
  where
    updateIfFound :: Client -> IO Client
    updateIfFound client
      | cpf client == searchCpf = do
          putStrLn $ "Atualizando informações do cliente: " ++ nameClient client
          putStrLn "Novo Nome: "
          newName <- getLine
          putStrLn "Nova Idade: "
          newAge <- readLn
          putStrLn "Novo Endereço: "
          newAddress <- getLine
          putStrLn "Novo Telefone: "
          newPhone <- getLine
          return client { 
            nameClient = newName,  
            age = newAge,            
            address = newAddress, 
            phone = newPhone 
          }
      | otherwise = return client

-- Função para deletar um cliente pelo CPF
deleteClient :: String -> IO ()
deleteClient searchCpf = do
  clients <- getAllClients
  let filteredClients = filter (\client -> cpf client /= searchCpf) clients
  writeFile "_customerDB.dat" (unlines $ map show filteredClients)
  putStrLn "** Cliente deletado com sucesso! **"

-- Função para adicionar uma venda a um cliente
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

n_salesClient :: Client -> Int
n_salesClient cliente = length (sales cliente)

levelClient :: Client -> String
levelClient cliente
   |(n_salesClient cliente) >= 50 = "Cliente nível ouro!"
   |(n_salesClient cliente) >= 25 = "Cliente nível prata!"
   |otherwise = "Cliente nível bronze!"

relatoryProduct :: Product -> String
relatoryProduct (Product nameProduct description category dateManufacture expirationDate price stock) = nameProduct ++ ": Descrição = " ++ show description ++ ", Categoria = " ++ show category ++ ", Data de Produção = " ++ show dateManufacture ++ ", Data de Validade = " ++ show expirationDate ++ ", Preço = " ++ show price ++ ", Estoque = " ++ show stock

relatoryProductClient :: Client -> String
relatoryProductClient cliente = intercalate "\n" (map relatoryProduct produtos)
   where produtos = concatMap products (sales cliente)
