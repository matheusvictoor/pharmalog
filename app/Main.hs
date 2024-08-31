module Main (main) where

import System.Directory (doesFileExist)
import Control.Monad (when, forever)
import Control.Concurrent (forkIO, newChan, readChan, writeChan, Chan)
import Controllers.MenuController (menu)

import Services.ChatService (startChat)
import Services.UserService (createUser, deleteUser, updateUser, getUserByName, getAllUsers)
import Services.ProductService (createProduct, deleteProduct, updateProduct, getProductById, getAllProducts, alertLowStockProducts, alertExpiringProducts)
import Services.ClientService (createClient, deleteClient, updateClient, getClientByCpf, getAllClients, viewClientInfo, addSaleToClient)
import Services.SaleService (createSale, deleteSale, updateSale, getSaleByClientId, getAllSales)

import System.IO (hFlush, stdout)
import Models.Message (Message(..))


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

  chatChannel <- newChan
  _ <- forkIO $ forever $ do
    message <- readChan chatChannel
    appendFile chatDB (sender message ++ ": " ++ content message ++ "\n")

  programLoop chatChannel

programLoop :: Chan Message -> IO ()
programLoop chatChannel = do
  option <- menu
  case option of
    1 -> createUser >> continue
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
      
    50 -> startChat chatChannel >> continue

    0 -> putStrLn "\nEncerrando o programa..."
    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      programLoop chatChannel
  where
    continue = do
      putStr "\nDeseja voltar ao menu? (s/n): "
      hFlush stdout
      result <- getLine
      if result == "s" then programLoop chatChannel else putStrLn "\nEncerrando o programa..."
