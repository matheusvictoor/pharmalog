module Controllers.MenuController (menu) where

menu :: IO Int
menu = do
  putStrLn "Pharmalog - Seu sistema de controle de farmacia (v1.0)"

  putStrLn "\n*****************************"
  putStrLn "\n***** Digite uma Opcao *****"
  putStrLn "\n******** USARIO *************"
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
  putStrLn "14.  Buscar"
  putStrLn "15.  Buscar Todos"

  putStrLn "\n******** CLIENTE ************"
  putStrLn "16.  Cadastrar"
  putStrLn "17.  Remover"
  putStrLn "18.  Atualizar"
  putStrLn "19.  Buscar"
  putStrLn "20.  Buscar Todos"
  putStrLn "50.  Chat"
  putStrLn "0 <- Sair\n"

  option <- readLn
  return option