module Main (main) where

import System.Directory (doesFileExist)
import Control.Monad (when, forever)
import Control.Concurrent (forkIO, newChan, readChan, Chan)
import Controllers.MenuController (menu)

import Services.ChatService (startChat)
import Services.UserService (menuUser)
import Services.ProductService (menuProduct)
import Services.ClientService (menuClient)
import Services.SaleService (menuSale)
import Services.RelatorioProduct(menuRelatorio)

import Models.Message (Message(..))
import GHC.IO.Encoding

main :: IO ()
main = do
  setLocaleEncoding utf8

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
    1 -> menuUser >> continue
    2 -> menuProduct >> continue
    3 -> menuSale >> continue
    4 -> menuClient >> continue
    5 -> startChat chatChannel >> continue
    6 -> menuRelatorio >> continue
    0 -> putStrLn "Encerrando o programa..."
    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      programLoop chatChannel
  where
    continue = programLoop chatChannel