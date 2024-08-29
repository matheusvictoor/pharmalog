module Controllers.MenuController (menu) where


import Services.ClientService (createClient, deleteClient, updateClient, getClientByCpf, getAllClients, addSaleToClient, viewClientInfo)
import Services.SaleService (createSale, deleteSale, updateSale, getSaleByClientId, getAllSales)
import Services.UserService (createUser, deleteUser, updateUser, getUserByName, getAllUsers, assignRoleToUser, createProductForAdmin, specificAdminFunctions, specificManagerFunctions, specificSellerFunctions)
import System.IO (hFlush, stdout)

menu :: IO Int
menu = do
  putStrLn "Pharmalog - Seu sistema de controle de farmacia (v1.0)"
  putStrLn "\n***********"
  putStrLn "\n*** Digite uma Opcao ***"

  -- Menu de Usuários
  putStrLn "\n**** USUARIO *****"
  putStrLn "1.   Cadastrar Usuário"
  putStrLn "2.   Remover Usuário"
  putStrLn "3.   Atualizar Usuário"
  putStrLn "4.   Buscar Usuário"
  putStrLn "5.   Buscar Todos os Usuários"
  putStrLn "6.   Atribuir Função a Usuário"
  putStrLn "7.   Funções Específicas do Administrador"
  putStrLn "8.   Funções Específicas do Gerente"
  putStrLn "9.   Funções Específicas do Vendedor"

  -- Menu de Produtos
  putStrLn "\n**** PRODUTO *****"
  putStrLn "10.  Cadastrar Produto"
  putStrLn "11.  Remover Produto"
  putStrLn "12.  Atualizar Produto"
  putStrLn "13.  Buscar Produto"
  putStrLn "14.  Buscar Todos os Produtos"
  putStrLn "15.  Alertar sobre Baixo Estoque"
  putStrLn "16.  Alertar sobre Produtos Perto de Vencer"

  -- Menu de Vendas
  putStrLn "\n**** VENDAS *****"
  putStrLn "17.  Cadastrar Venda"
  putStrLn "18.  Remover Venda"
  putStrLn "19.  Atualizar Venda"
  putStrLn "20.  Buscar Venda"
  putStrLn "21.  Buscar Todas as Vendas"

  -- Menu de Clientes
  putStrLn "\n**** CLIENTE ****"
  putStrLn "22.  Cadastrar Cliente"
  putStrLn "23.  Remover Cliente"
  putStrLn "24.  Atualizar Cliente"
  putStrLn "25.  Buscar Cliente"
  putStrLn "26.  Buscar Todos os Clientes"
  putStrLn "27.  Visualizar Informações do Cliente" -- Nova funcionalidade adicionada
  putStrLn "28.  Adicionar Venda a Cliente"

  -- Menu de Chat
  putStrLn "\n**** CHAT ****"
  putStrLn "50.  Simular Chat"

  -- Sair do Sistema
  putStrLn "\n0.  Sair\n"
  putStr "Escolha uma opção: "
  hFlush stdout
  readLn
