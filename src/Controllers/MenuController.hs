module Controllers.MenuController (menu) where

import Services.ClientService ()
import Services.SaleService ()
import Services.UserService ()
import System.IO (hFlush, stdout)

menu :: IO Int
menu = do
  putStrLn "\nPharmalog - Seu sistema de controle de farmacia (v1.0)"
  
  putStrLn "******************************************************"
  putStrLn "********************** Bem-Vindo *********************\n"

  putStrLn "1.   Usuários"
  putStrLn "2.   Produtos"
  putStrLn "3.   Vendas"
  putStrLn "4.   Clientes"
  putStrLn "5.   Chat"
  putStrLn "6.   Relatórios"
  putStrLn "0 <- Sair\n"

  putStr ("Escolha uma opção -> ")

  hFlush stdout
  option <- readLn
  putStrLn ""
  
  return option