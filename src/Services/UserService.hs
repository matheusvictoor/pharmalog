{-# LANGUAGE BangPatterns #-}
module Services.UserService (createUser, getAllUsers, getUserByName, updateUser, deleteUser, assignRoleToUser,specificAdminFunctions, specificManagerFunctions, specificSellerFunctions, menuUser) where

import Models.User
import Data.List (find, deleteBy)
import Data.Time.Clock (UTCTime)
import Control.DeepSeq (deepseq)
import System.IO (hFlush, stdout, withFile, IOMode(ReadMode, WriteMode), hGetContents, hPutStr)

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
    _               -> fail "Função inválida! Escolha entre: Administrador, Gerente ou Vendedor."

-- Retorna todos os usuários do arquivo _userDB.dat
getAllUsers :: IO [User]
getAllUsers = do
  contents <- readFile "_userDB.dat"
  let users = map (userData . read) (lines contents) :: [User]
  return users

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

-- Atualiza as informações de um usuário
updateUser :: String -> IO ()
updateUser searchName = do
  -- Ler o conteúdo do arquivo garantindo que seja completamente avaliado
  contents <- withFile "_userDB.dat" ReadMode $ \handle -> do
    c <- hGetContents handle
    c `deepseq` return c  -- Força a avaliação completa do conteúdo
  
  let users = lines contents
  let updatedUsers = map updateIfFound users
  
  -- Escrever o conteúdo atualizado de volta ao arquivo
  withFile "_userDB.dat" WriteMode $ \handle -> do
    hPutStr handle (unlines updatedUsers)
  
  putStrLn "** Usuário atualizado com sucesso! **"
  where
    updateIfFound line =
      let user = userData (read line :: Index User)
      in if name user == searchName
         then show (Index (index (read line :: Index User)) (User
            { name = searchName
            , password = "NovaSenha"
            , role = role user
            }))
         else line

deleteUser :: String -> IO ()
deleteUser searchName = do
  -- Abrindo o arquivo para leitura
  contents <- withFile "_userDB.dat" ReadMode $ \handle -> do
    c <- hGetContents handle
    c `deepseq` return c  -- Força a leitura completa do conteúdo
  
  let users = lines contents
  let filteredUsers = filter (\line -> name (userData (read line :: Index User)) /= searchName) users
  
  -- Abrindo o arquivo para escrita (substituindo o conteúdo)
  withFile "_userDB.dat" WriteMode $ \handle -> do
    hPutStr handle (unlines filteredUsers)
  
  putStrLn "** Usuário deletado com sucesso! **"

-- Função para atribuir uma função a um usuário existente
assignRoleToUser :: String -> Role -> IO ()
assignRoleToUser searchName newRole = do
  contents <- readFile "_userDB.dat"
  let users = lines contents
  let updatedUsers = map assignRoleIfFound users
  writeFile "_userDB.dat" (unlines updatedUsers)
  putStrLn "** Função atribuída ao usuário com sucesso! **"
  where
    assignRoleIfFound line =
      let user = userData (read line :: Index User)
      in if name user == searchName
         then show (Index (index (read line :: Index User)) (User
            { name = name user
            , password = password user
            , role = newRole
            }))
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
  putStrLn "2.  Buscar um usuário por nome"
  putStrLn "3.  Buscar todos os usuários"
  putStrLn "4.  Atualizar um usuário"
  putStrLn "5.  Deletar um usuário"
  putStrLn "6.  Atribuir cargo a um usuário existente"
  putStrLn "0 <- Voltar"

  putStr "Opção -> "
  hFlush stdout

  option <- getLine

  case option of
    "1" -> createUser
    -- "2" -> getUserById
    "3" -> getUserByName
    -- "4" -> getAllUsers
    -- "5" -> updateUser
    -- "6" -> deleteUser
    -- "7" -> assignRoleToUser
    "0" -> putStrLn "\n<---"
    _   -> putStrLn "Opção inválida. Tente novamente." >> menuUser
  putStrLn ""

