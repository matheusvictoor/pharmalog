module Main (main) where

import System.Directory (doesFileExist)
import Control.Monad (when, forever)
import Control.Concurrent (forkIO, newChan, readChan, writeChan, Chan)
import Controllers.MenuController (menu)
import Services.UserService (createUser)
import Services.ChatService (startChat)
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
    
    50 -> startChat chatChannel >> continue

    0 -> putStrLn "\nEncerrando o programa..."
    _ -> programLoop chatChannel
  where
    continue = do
      putStr "\nDeseja voltar ao menu? (s/n): "
      hFlush stdout
      result <- getLine
      if result == "s" then programLoop chatChannel else putStrLn "\nEncerrando o programa..."

