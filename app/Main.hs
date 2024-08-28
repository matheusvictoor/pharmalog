module Main (main) where

import System.Directory (doesFileExist)
import Control.Monad (when)
import Controllers.MenuController (menu)
import Services.UserService (createUser, deleteUser, updateUser, getUserByName, getAllUsers)
import Services.ProductService (createProduct, deleteProduct, updateProduct, getProductById, getAllProducts)
import Services.ClientService (createClient, deleteClient, updateClient, getClientByCpf, getAllClients)
import Services.SaleService (createSale, deleteSale, updateSale, getSaleByClientId, getAllSales)
import Services.ChatService (simuleChat)

main :: IO ()
main = do
  let userDB = "_userDB.dat"
  let productDB = "_productDB.dat"
  let customerDB = "_customerDB.dat"
  let saleDB = "_saleDB.dat"
  let chatDB = "_chatDB.dat"
      
  existUsers <- doesFileExist userDB
  existProducts <- doesFileExist productDB
  existCustomers <- doesFileExist customerDB
  existSales <- doesFileExist saleDB
  existChat <- doesFileExist chatDB

  when (not existUsers) (writeFile userDB "")
  when (not existProducts) (writeFile productDB "")
  when (not existCustomers) (writeFile customerDB "")
  when (not existSales) (writeFile saleDB "")
  when (not existChat) (writeFile chatDB "")

  programLoop

programLoop :: IO ()
programLoop = do
  option <- menu
  case option of
    -- Usuário
    1 -> createUser >> continue
    2 -> do
      putStrLn "Nome do usuário para remover: "
      name <- getLine
      deleteUser name
      continue
    3 -> do
      putStrLn "Nome do usuário para atualizar: "
      name <- getLine
      updateUser name
      continue
    4 -> do
      putStrLn "Nome do usuário para buscar: "
      name <- getLine
      result <- getUserByName name
      case result of
        Just user -> print user
        Nothing -> putStrLn "Usuário não encontrado."
      continue
    5 -> getAllUsers >>= mapM_ print >> continue

    -- Produto
    6 -> createProduct >> continue
    7 -> do
      putStrLn "Nome do produto para remover: "
      name <- getLine
      deleteProduct name
      continue
    8 -> do
      putStrLn "Nome do produto para atualizar: "
      name <- getLine
      updateProduct name
      continue
    9 -> do
      putStrLn "Nome do produto para buscar: "
      name <- getLine
      result <- getProductById name
      case result of
        Just product -> print product
        Nothing -> putStrLn "Produto não encontrado."
      continue
    10 -> getAllProducts >>= mapM_ print >> continue

    -- Venda
    11 -> createSale >> continue
    12 -> do
      putStrLn "ID do cliente da venda para remover: "
      clientId <- readLn
      deleteSale clientId
      continue
    13 -> do
      putStrLn "ID do cliente da venda para atualizar: "
      clientId <- readLn
      updateSale clientId
      continue
    14 -> do
      putStrLn "ID do cliente da venda para buscar: "
      clientId <- readLn
      result <- getSaleByClientId clientId
      case result of
        Just sale -> print sale
        Nothing -> putStrLn "Venda não encontrada."
      continue
    15 -> getAllSales >>= mapM_ print >> continue

    -- Cliente
    16 -> createClient >> continue
    17 -> do
      putStrLn "CPF do cliente para remover: "
      cpf <- getLine
      deleteClient cpf
      continue
    18 -> do
      putStrLn "CPF do cliente para atualizar: "
      cpf <- getLine
      updateClient cpf
      continue
    19 -> do
      putStrLn "CPF do cliente para buscar: "
      cpf <- getLine
      result <- getClientByCpf cpf
      case result of
        Just client -> print client
        Nothing -> putStrLn "Cliente não encontrado."
      continue
    20 -> getAllClients >>= mapM_ print >> continue

    -- Chat
    50 -> simuleChat >> continue

    -- Encerrar
    0 -> putStrLn "Encerrando o programa...."

    _ -> programLoop
  where
    continue = do
      putStrLn "\nDeseja voltar ao menu? (s/n)"
      result <- getLine
      if result == "s" then programLoop else putStrLn "Encerrando o programa..."
