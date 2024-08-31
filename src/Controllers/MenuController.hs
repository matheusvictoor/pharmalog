module Controllers.MenuController (menu) where

import Services.ClientService ()
import Services.SaleService ()
import Services.UserService ()
import System.IO (hFlush, stdout)

menu :: IO Int
menu = do
  putStrLn "Pharmalog - Seu sistema de controle de farmacia (v1.0)"
  
  putStrLn "\n******************************"
  putStrLn "\n****** Digite uma Opcao ******"

  putStrLn "\n********* USARIO *************"
  putStrLn "1.   Cadastrar"
  putStrLn "2.   Remover"
  putStrLn "3.   Atualizar"
  putStrLn "4.   Buscar"
  putStrLn "5.   Buscar Todos os Usuários"
  putStrLn "6.   Atribuir Função a Usuário"
  putStrLn "7.   Funções Específicas do Administrador"
  putStrLn "8.   Funções Específicas do Gerente"
  putStrLn "9.   Funções Específicas do Vendedor"

  putStrLn "\n********* PRODUTO *************"
  putStrLn "10.  Cadastrar"
  putStrLn "11.  Remover"
  putStrLn "12.  Atualizar"
  putStrLn "13.  Buscar"
  putStrLn "14.  Buscar Todos os Produtos"
  putStrLn "15.  Alertar sobre Baixo Estoque"
  putStrLn "16.  Alertar sobre Produtos Perto de Vencer"

  putStrLn "\n********* VENDAS **************"
  putStrLn "17.  Cadastrar"
  putStrLn "18.  Remover"
  putStrLn "19.  Atualizar"
  putStrLn "20.  Buscar"
  putStrLn "21.  Buscar Todas as Vendas"

  putStrLn "\n********* CLIENTE *************"
  putStrLn "22.  Cadastrar"
  putStrLn "23.  Remover"
  putStrLn "24.  Atualizar"
  putStrLn "25.  Buscar"
  putStrLn "26.  Buscar Todos os Clientes"

  putStrLn "\n********* RELATORIOS *************"
  putStrLn "27.  Relatório por faixa de preço"
  putStrLn "28.  Relatório por categoria"
  putStrLn "29.  Relatório por faixa de estoque"

  putStrLn "\n********* CHAT *************"
  putStrLn "50.  Chat"

  putStrLn "\n0 <- Sair\n"

  putStr ("Escolha uma opção -> ")

  hFlush stdout
  option <- readLn
  putStrLn ""
  
  return option