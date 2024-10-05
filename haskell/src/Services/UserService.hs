{-# LANGUAGE BangPatterns #-}
module Services.UserService (createUser, getAllUsers, getUserByCpf, getUserByName, updateUser, deleteUser, assignRoleToUser,specificAdminFunctions, specificManagerFunctions, specificSellerFunctions, menuUser) where

import Models.User
import Data.List (find)
import System.IO (hFlush, stdout, hClose, hPutStr, openTempFile)
import System.Directory (removeFile, renameFile)

data Index a = Index { index :: Int, userData :: a } deriving (Show, Read)

-- Cria um novo usuário e o salva no arquivo _userDB.dat
createUser :: IO ()
createUser = do
  putStrLn "CPF: "
  cpfInput <- getLine
  cpfExists <- userExistCpf cpfInput

  if cpfExists
    then putStrLn "\n** CPF já cadastrado! **"
    else do
      !userId <- fmap (length . lines) (readFile "_userDB.dat")
      user <- User
        <$> (putStrLn "\nNome: " >> getLine)
        <*> return cpfInput
        <*> (putStrLn "Password: " >> getLine)
        <*> (putStrLn "Função (Administrador | Gerente | Vendedor): " >> readRole)
      appendFile "_userDB.dat" (show (Index (1+userId) user) ++ "\n")
      putStrLn "\n** Usuário cadastrado com sucesso! **"

userExistCpf :: String -> IO Bool
userExistCpf cpfInput = do
  contents <- readFile "_userDB.dat"
  let users = map (read :: String -> Index User) (lines contents)
  return $ any (\u -> cpf (userData u) == cpfInput) users

-- Função para ler o papel do usuário
readRole :: IO Role
readRole = do
  roleStr <- getLine
  case roleStr of
    "Administrador" -> return Administrador
    "Gerente" -> return Gerente
    "Vendedor" -> return Vendedor
    _ -> do
      putStrLn "\nCargo inválido! Escolha entre: Administrador, Gerente ou Vendedor."
      readRole

-- Retorna todos os usuários do arquivo _userDB.dat
getAllUsers :: IO [User]
getAllUsers = do
  contents <- readFile "_userDB.dat"
  let users = map (userData . read) (lines contents) :: [User]
  return users

showAllUsers :: IO ()
showAllUsers = do
  contents <- readFile "_userDB.dat"
  let users = map (read :: String -> Index User) (lines contents)
  putStrLn "\nUsuários cadastrados:"
  mapM_ printUser users

getUserByCpf :: IO ()
getUserByCpf = do
  putStrLn "CPF do usuário para buscar: "
  searchCpf <- getLine
  userExists <- userExistCpf searchCpf
  if userExists
    then do
      contents <- readFile "_userDB.dat"
      let users = map (read :: String -> Index User) (lines contents)
      case find (\c -> cpf (userData c) == searchCpf) users of
        Just u -> do
          putStrLn $ "\nInformações do usuário:"
          printUser u
        Nothing -> putStrLn "\nUsuário não encontrado."
    else putStrLn "\nUsuário não encontrado."

-- Busca um usuário pelo nome no arquivo _userDB.dat
getUserByName :: IO ()
getUserByName = do
  putStrLn "Nome do usuário para buscar: "
  searchName <- getLine
  contents <- readFile "_userDB.dat"
  let users = map (read :: String -> Index User) (lines contents)
  case find (\u -> name (userData u) == searchName) users of
    Just u -> do
      putStrLn $ "\nInformações do usuário:"
      printUser u
    Nothing -> putStrLn "\nUsuário não encontrado."

updateUser :: IO ()
updateUser = do
  putStrLn "CPF do usuário para atualizar: "
  userCpf <- getLine
  userExists <- userExistCpf userCpf
  if userExists
    then do
      contents <- readFile "_userDB.dat"
      let users = lines contents
      updatedUsers <- mapM (updateIfFound userCpf) users
      writeFile "_userDB.dat" (unlines updatedUsers)
      putStrLn "\nUsuário atualizado com sucesso! "
    else putStrLn "\nUsuário não encontrado."
  where
    updateIfFound :: String -> String -> IO String
    updateIfFound userCpf line =
      let userRecord = read line :: Index User
          userCpfInRecord  = cpf (userData userRecord)
      in if userCpfInRecord  == userCpf
        then do
          putStrLn "\nInforme os novos dados do usuário:"

          putStrLn "Senha:"
          newPassword <- getLine

          putStrLn "Cargo (Administrador | Gerente | Vendedor):"
          newRole <- readRole

          let updatedUser = User
                { name = name (userData userRecord)
                , cpf = userCpf
                , password = newPassword
                , role = newRole
                }

          putStrLn "\nUsuário atualizado:"
          printUser (Index (index userRecord) updatedUser)

          return (show (Index (index userRecord) updatedUser))
        else return line

deleteUser :: IO ()
deleteUser = do
  putStrLn "CPF do usuário a ser deletado: "
  userCpf <- getLine
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- readFile "_userDB.dat"
  let users = lines contents
      filteredUsers = filter (\line -> cpf (userData (read line :: Index User)) /= userCpf) users
  if length filteredUsers == length users
    then putStrLn "Usuário não encontrado."
    else do
      hPutStr tempHandle (unlines filteredUsers)
      hClose tempHandle
      removeFile "_userDB.dat"
      renameFile tempName "_userDB.dat"
      putStrLn "\n** Usuário deletado com sucesso! **"

--função para atribuir uma função a um usuário existente
assignRoleToUser :: IO ()
assignRoleToUser = do
  putStrLn "\nCPF do usuário para atribuir função: "
  userCpf <- getLine
  userExists <- userExistCpf userCpf
  if userExists
    then do
      contents <- readFile "_userDB.dat"
      let users = lines contents
      updatedUsers <- mapM (updateIfFound userCpf) users
      writeFile "_userDB.dat" (unlines updatedUsers)
      putStrLn "\nFunção atribuída ao usuário com sucesso! "
    else putStrLn "\nUsuário não encontrado."
  where
    updateIfFound :: String -> String -> IO String
    updateIfFound userCpf line =
      let userRecord = read line :: Index User
          userCpfInRecord  = cpf (userData userRecord)
      in if userCpfInRecord  == userCpf
        then do
          putStrLn "Cargo (Administrador | Gerente | Vendedor):"
          newRole <- readRole

          let updatedUser = User
                { name = name (userData userRecord)
                , cpf = userCpf
                , password = password (userData userRecord)
                , role = newRole
                }

          putStrLn "\nUsuário atualizado:"
          printUser (Index (index userRecord) updatedUser)

          return (show (Index (index userRecord) updatedUser))
        else return line

-- Funções específicas do Administrador
specificAdminFunctions :: IO ()
specificAdminFunctions = do
  putStrLn "Administrador - Acesso completo ao sistema."

-- Funções específicas do Gerente
specificManagerFunctions :: IO ()
specificManagerFunctions = do
  putStrLn "Gerente - Funções específicas para gerenciar clientes e visualizar relatórios."

specificSellerFunctions :: IO ()
specificSellerFunctions = do
  putStrLn "Vendedor - Funções para alterar status de produto e visualizar relatórios de clientes."

printUser :: Index User -> IO ()
printUser (Index idx user) = do
  putStrLn $ "ID: " ++ show idx
  putStrLn $ "CPF: " ++ cpf user
  putStrLn $ "Nome: " ++ name user
  putStrLn $ "Senha: **********"
  putStrLn $ "Cargo: " ++ show (role user)
  putStrLn "----------------------------------------"

menuUser :: IO ()
menuUser = do
  putStrLn "\nSelecione uma opção:"
  putStrLn "1.  Cadastrar um novo usuário"
  putStrLn "2.  Buscar um usuário por CPF"
  putStrLn "3.  Buscar um usuário por nome"
  putStrLn "4.  Buscar todos os usuários"
  putStrLn "5.  Atualizar um usuário"
  putStrLn "6.  Deletar um usuário"
  putStrLn "7.  Atribuir cargo a um usuário existente"
  putStrLn "0 <- Voltar"

  putStr "\nOpção -> "
  hFlush stdout

  option <- getLine

  case option of
    "1" -> createUser
    "2" -> getUserByCpf
    "3" -> getUserByName
    "4" -> showAllUsers
    "5" -> updateUser
    "6" -> deleteUser
    "7" -> assignRoleToUser
    "0" -> putStrLn "\n<---"
    _   -> putStrLn "Opção inválida. Tente novamente." >> menuUser
  putStrLn ""

