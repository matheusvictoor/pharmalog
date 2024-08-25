module Main (main) where

import System.Directory (doesFileExist)
import Control.Monad (when)
import Controllers.MenuController (menu)
import Services.UserService (createUser)

main :: IO ()
main = do
  let userDB = "_userDB.dat"
  let productDB = "_productDB.dat"
  let customerDB = "_customerDB.dat"
  let saleDB = "_saleDB.dat"
      
  existUsers <- doesFileExist userDB
  existProducts <- doesFileExist productDB
  existCustomers <- doesFileExist customerDB
  existSales <- doesFileExist saleDB

  when (not existUsers) (writeFile userDB "")
  when (not existProducts) (writeFile productDB "")
  when (not existCustomers) (writeFile customerDB "")
  when (not existSales) (writeFile saleDB "")

  programLoop

programLoop :: IO ()
programLoop = do
  option <- menu
  case option of
    1 -> createUser >> continue
    0 -> putStrLn "Encerrando o programa...."
    _ -> programLoop
  where
    continue = do
      putStrLn "\nDeseja voltar ao menu? (s/n)"
      result <- getLine
      if result == "s" then programLoop else putStrLn "Encerrando o programa..."
