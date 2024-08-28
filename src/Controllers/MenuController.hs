module Controllers.MenuController (menu) where

import Services.ProductService (createProduct, deleteProduct, updateProduct, getProductById, getAllProducts)
import Services.ClientService (createClient, deleteClient, updateClient, getClientByCpf, getAllClients)
import Services.SaleService (createSale, deleteSale, updateSale, getSaleByClientId, getAllSales)
import Services.UserService (createUser, deleteUser, updateUser, getUserByName, getAllUsers)
import System.IO (hFlush, stdout)

menu :: IO ()
menu = do
  option <- showMenu
  processOption option

showMenu :: IO Int
showMenu = do
  putStrLn "Pharmalog - Seu sistema de controle de farmacia (v1.0)"
  putStrLn "\n*****************************"
  putStrLn "\n***** Digite uma Opcao *****"
  putStrLn "\n******** USUARIO *************"
  putStrLn "1.   Cadastrar"
  putStrLn "2.   Remover"
  putStrLn "3.   Atualizar"
  putStrLn "4.   Buscar"
  putStrLn "5.   Buscar Todos"

  putStrLn "\n******** MEDICAMENTO ********"
  putStrLn "6.   Cadastrar"
  putStrLn "7.   Remover"
  putStrLn "8.   Atualizar"
  putStrLn "9.   Buscar"
  putStrLn "10.  Buscar Todos"

  putStrLn "\n******** VENDAS *************"
  putStrLn "11.  Cadastrar"
  putStrLn "12.  Remover"
  putStrLn "13.  Atualizar"
  putStrLn "14.  Buscar"
  putStrLn "15.  Buscar Todas"

  putStrLn "\n******** CLIENTE ************"
  putStrLn "16.  Cadastrar"
  putStrLn "17.  Remover"
  putStrLn "18.  Atualizar"
  putStrLn "19.  Buscar"
  putStrLn "20.  Buscar Todos"
  putStrLn "50.  Chat"
  putStrLn "0 <- Sair\n"
  putStr "Escolha uma opção: "
  hFlush stdout
  readLn

processOption :: Int -> IO ()
processOption option = case option of
  -- Opções de Usuário
  1  -> createUser
  2  -> do
    putStrLn "Nome do usuário para remover: "
    name <- getLine
    deleteUser name
  3  -> do
    putStrLn "Nome do usuário para atualizar: "
    name <- getLine
    updateUser name
  4  -> do
    putStrLn "Nome do usuário para buscar: "
    name <- getLine
    result <- getUserByName name
    case result of
      Just user -> print user
      Nothing -> putStrLn "Usuário não encontrado."
  5  -> do
    users <- getAllUsers
    mapM_ print users

  -- Opções de Produto (Medicamento)
  6  -> createProduct
  7  -> do
    putStrLn "Nome do produto para remover: "
    name <- getLine
    deleteProduct name
  8  -> do
    putStrLn "Nome do produto para atualizar: "
    name <- getLine
    updateProduct name
  9  -> do
    putStrLn "Nome do produto para buscar: "
    name <- getLine
    result <- getProductById name
    case result of
      Just product -> print product
      Nothing -> putStrLn "Produto não encontrado."
  10 -> do
    products <- getAllProducts
    mapM_ print products

  -- Opções de Venda
  11 -> createSale
  12 -> do
    putStrLn "ID do cliente da venda para remover: "
    clientId <- readLn
    deleteSale clientId
  13 -> do
    putStrLn "ID do cliente da venda para atualizar: "
    clientId <- readLn
    updateSale clientId
  14 -> do
    putStrLn "ID do cliente da venda para buscar: "
    clientId <- readLn
    result <- getSaleByClientId clientId
    case result of
      Just sale -> print sale
      Nothing -> putStrLn "Venda não encontrada."
  15 -> do
    sales <- getAllSales
    mapM_ print sales

  -- Opções de Cliente
  16 -> createClient
  17 -> do
    putStrLn "CPF do cliente para remover: "
    cpf <- getLine
    deleteClient cpf
  18 -> do
    putStrLn "CPF do cliente para atualizar: "
    cpf <- getLine
    updateClient cpf
  19 -> do
    putStrLn "CPF do cliente para buscar: "
    cpf <- getLine
    result <- getClientByCpf cpf
    case result of
      Just client -> print client
      Nothing -> putStrLn "Cliente não encontrado."
  20 -> do
    clients <- getAllClients
    mapM_ print clients

  50 -> putStrLn "Chat"
  0  -> putStrLn "Saindo do sistema..."

  _  -> do
    putStrLn "Opção inválida. Tente novamente."
    menu
