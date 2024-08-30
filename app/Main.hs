module Main (main) where

import System.Directory (doesFileExist)
import Control.Monad (when)
import Controllers.MenuController (menu)
import Services.UserService (createUser, deleteUser, updateUser, getUserByName, getAllUsers)
import Services.ProductService (createProduct, deleteProduct, updateProduct, getProductById, getAllProducts, alertLowStockProducts, alertExpiringProducts)
import Services.ClientService (createClient, deleteClient, updateClient, getClientByCpf, getAllClients, viewClientInfo, addSaleToClient)

import Services.SaleService (createSale, deleteSale, updateSale, getSaleByClientId, getAllSales)
import Services.ChatService (simuleChat)
import Services.UserService (createUser, deleteUser, updateUser, getUserByName, getAllUsers)


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
  processOption option

processOption :: Int -> IO ()
processOption option = case option of
    1  -> createUser >> getLine >> continue
    2  -> do
      putStrLn "Nome do usuário para remover: "
      name <- getLine
      deleteUser name
      continue
    3  -> do
      putStrLn "Nome do usuário para atualizar: "
      name <- getLine
      updateUser name
      continue
    4  -> do
      putStrLn "Nome do usuário para buscar: "
      name <- getLine
      result <- getUserByName name
      case result of
        Just user -> print user
        Nothing -> putStrLn "Usuário não encontrado."
      continue
    5  -> do
      users <- getAllUsers
      if null users
        then putStrLn "Nenhum usuário encontrado."
        else mapM_ print users
      continue


    10  -> createProduct >> continue
    11  -> do
      putStrLn "Nome do produto para remover: "
      productName <- getLine
      deleteProduct productName
      continue
    12  -> do
      putStrLn "Nome do produto para atualizar: "
      productName <- getLine
      updateProduct productName
      continue
    13  -> do
      putStrLn "Nome do produto para buscar: "
      productName <- getLine
      result <- getProductById productName
      case result of
        Just prod -> print prod
        Nothing -> putStrLn "Produto não encontrado."
      continue
    14 -> do
      products <- getAllProducts
      mapM_ print products
      continue
    15 -> do
      putStrLn "Digite o limite de estoque para alerta: "
      limit <- readLn
      alertLowStockProducts limit
      continue
    16 -> do
      putStrLn "Digite a quantidade de dias para alerta de vencimento: "
      daysBefore <- readLn
      alertExpiringProducts daysBefore
      continue

    17 -> createSale >> continue
    18 -> do
      putStrLn "ID do cliente da venda para remover: "
      clientId <- readLn
      deleteSale clientId
      continue
    19 -> do
      putStrLn "ID do cliente da venda para atualizar: "
      clientId <- readLn
      updateSale clientId
      continue
    20 -> do
      putStrLn "ID do cliente da venda para buscar: "
      clientId <- readLn
      result <- getSaleByClientId clientId
      case result of
        Just sale -> print sale
        Nothing -> putStrLn "Venda não encontrada."
      continue
    21 -> do
      sales <- getAllSales
      mapM_ print sales
      continue

    22 -> createClient >> continue
    23 -> do
      putStrLn "CPF do cliente para remover: "
      cpf <- getLine
      deleteClient cpf
      continue
    24 -> do
      putStrLn "CPF do cliente para atualizar: "
      cpf <- getLine
      updateClient cpf
      continue
    25 -> do
      putStrLn "CPF do cliente para buscar: "
      cpf <- getLine
      result <- getClientByCpf cpf
      case result of
        Just client -> print client
        Nothing -> putStrLn "Cliente não encontrado."
      continue
    26  -> createClient >> continue
    27 -> do
      putStrLn "CPF do cliente para visualizar: "
      cpf <- getLine
      viewClientInfo cpf
      continue
    28 -> do
      putStrLn "CPF do cliente para adicionar venda: "
      cpf <- getLine
      putStrLn "ID da venda a ser adicionada: "
      sale <- readLn
      addSaleToClient cpf sale
      continue

    50 -> simuleChat >> continue

    0  -> putStrLn "Saindo do sistema..."

    _  -> do
      putStrLn "Opção inválida. Tente novamente."
      programLoop

  where
    continue = programLoop
