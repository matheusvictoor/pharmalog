{-# LANGUAGE BangPatterns #-}
module Services.UserService (createUser, getAllUsers, getUserById, getUserByName, updateUser, deleteUser, assignRoleToUser,specificAdminFunctions, specificManagerFunctions, specificSellerFunctions, menuUser) where

import Models.User
import Data.List (find, isPrefixOf)
import Data.Time.Clock (UTCTime)
import Control.DeepSeq (deepseq)
import System.IO (hFlush, stdout, withFile, hClose, hGetContents, hPutStr, openTempFile)
import System.Directory (removeFile, renameFile)

data Index a = Index { index :: Int, userData :: a } deriving (Show, Read)

-- Cria um novo usuário e o salva no arquivo _userDB.dat
createUser :: IO ()
createUser = do
  !userId <- fmap (length . lines) (readFile "_userDB.dat")
  user <- User
    <$> (putStrLn "\nNome: " >> getLine)
    <*> (putStrLn "Password: " >> getLine)
    <*> (putStrLn "Função (Administrador | Gerente | Vendedor): " >> readRole)
  appendFile "_userDB.dat" (show (Index (1+userId) user) ++ "\n")
  putStrLn "** Usuário cadastrado com sucesso! **"

-- Função para ler o papel do usuário
readRole :: IO Role
readRole = do
  roleStr <- getLine
  case roleStr of
    "Administrador" -> return Administrador
    "Gerente"       -> return Gerente
    "Vendedor"      -> return Vendedor
    _               -> do
      putStrLn "Cargo inválido! Escolha entre: Administrador, Gerente ou Vendedor."
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

getUserById :: IO ()
getUserById = do
  putStrLn "ID do usuário para buscar: "
  searchId <- readLn
  contents <- readFile "_userDB.dat"
  let users = map (read :: String -> Index User) (lines contents)
  case find (\u -> index u == searchId) users of
    Just u -> do
      putStrLn $ "\nInformações do usuário:"
      printUser u
    Nothing -> putStrLn "Usuário não encontrado."

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
    Nothing -> putStrLn "Usuário não encontrado."

updateUser :: IO ()
updateUser = do
  contents <- readFile "_userDB.dat"
  putStrLn "ID do usuário para atualizar: "
  userId <- readLn
  let users = lines contents
  updatedUsers <- mapM (updateIfFound userId) users
  writeFile "_userDB.dat" (unlines updatedUsers)
  putStrLn "Usuário atualizado com sucesso! "
  where
    updateIfFound :: Int -> String -> IO String
    updateIfFound userId line =
      let userRecord = read line :: Index User
          userIndex = index userRecord
      in if userIndex == userId
        then do
          putStrLn "Informe os novos dados do usuário:"
          putStrLn "Nome:"
          newName <- getLine

          putStrLn "Senha:"
          newPassword <- getLine

          putStrLn "Cargo (Administrador | Gerente | Vendedor):"
          newRole <- readRole

          let updatedUser = User
                { name = newName
                , password = newPassword
                , role = newRole
                }

          putStrLn "\nUsuário atualizado:"
          printUser (Index userIndex updatedUser)

          return (show (Index userIndex updatedUser))
        else return line

deleteUser :: IO ()
deleteUser = do
  putStrLn "ID do usuário a ser deletado: "
  userId <- getLine
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- readFile "_userDB.dat"
  let users = lines contents
      filteredUsers = filter (not . isPrefixOf ("Index {index = " ++ userId ++ ",") ) users
  hPutStr tempHandle (unlines filteredUsers)
  hClose tempHandle
  removeFile "_userDB.dat"
  renameFile tempName "_userDB.dat"
  putStrLn "** Usuário deletado com sucesso! **"

-- Função para atribuir uma função a um usuário existente
assignRoleToUser :: IO ()
assignRoleToUser = do
  putStrLn "\nID do usuário para atribuir função: "
  userId <- readLn
  putStrLn "\nCargo (Administrador | Gerente | Vendedor):"
  newRole <- readRole
  contents <- readFile "_userDB.dat"
  
  let users = lines contents
      userExists = any (isPrefixOf ("Index {index = " ++ show userId ++ ",")) users
  
  if not userExists
    then putStrLn $ "\nErro: Usuário com ID " ++ show userId ++ " não encontrado."
    else do
      (tempName, tempHandle) <- openTempFile "." "temp"
      let updatedUsers = map (assignRoleIfFound userId newRole) users
      
      hPutStr tempHandle (unlines updatedUsers)
      hClose tempHandle
      removeFile "_userDB.dat"
      renameFile tempName "_userDB.dat"
      
      putStrLn "\n** Função atribuída ao usuário com sucesso! **"
      
      let updatedUser = filter (isPrefixOf ("Index {index = " ++ show userId ++ ",")) updatedUsers
      case updatedUser of
        [userLine] -> printUser (read userLine :: Index User)
        _          -> putStrLn "\nErro ao mostrar o usuário atualizado."
  where
    assignRoleIfFound :: Int -> Role -> String -> String
    assignRoleIfFound userId newRole line =
      let userIndex = index (read line :: Index User)
      in if userIndex == userId
        then show (Index userIndex (User (name (userData (read line :: Index User))) (password (userData (read line :: Index User))) newRole))
        else line

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
  putStrLn $ "Nome: " ++ name user
  putStrLn $ "Senha: **********"
  putStrLn $ "Cargo: " ++ show (role user)
  putStrLn "----------------------------------------"

menuUser :: IO ()
menuUser = do
  putStrLn "Selecione uma opção:"
  putStrLn "1.  Cadastrar um novo usuário"
  putStrLn "2.  Buscar um usuário por ID"
  putStrLn "3.  Buscar um usuário por nome"
  putStrLn "4.  Buscar todos os usuários"
  putStrLn "5.  Atualizar um usuário"
  putStrLn "6.  Deletar um usuário"
  putStrLn "7.  Atribuir cargo a um usuário existente"
  putStrLn "0 <- Voltar"

  putStr "Opção -> "
  hFlush stdout

  option <- getLine

  case option of
    "1" -> createUser
    "2" -> getUserById
    "3" -> getUserByName
    "4" -> showAllUsers
    "5" -> updateUser
    "6" -> deleteUser
    "7" -> assignRoleToUser
    "0" -> putStrLn "\n<---"
    _   -> putStrLn "Opção inválida. Tente novamente." >> menuUser
  putStrLn ""

