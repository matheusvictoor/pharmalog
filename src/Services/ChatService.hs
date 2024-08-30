module Services.ChatService (startChat) where

import Control.Concurrent (Chan, writeChan)
import Data.Unique (newUnique, hashUnique)
import Models.Message (Message(..))
import System.IO (hFlush, stdout)

startChat :: Chan Message -> IO ()
startChat chatChannel = do
    putStrLn $ "\n**************************** Bem-vindo ao Chat da Pharmalog **********************************\n"
    putStr "\nDigite seu nome, cliente: "
    hFlush stdout
    clientName <- getLine
    putStrLn "\nCliente, digite 'sair' para encerrar o chat!"
    putStrLn $ "\n*************************** Chat iniciado, digite sua mensagem ********************************\n"

    simuleChat clientName chatChannel

simuleChat :: String -> Chan Message -> IO ()
simuleChat clientName channel = do
    uid <- newUnique
    let clientId = show (hashUnique uid)
    chatLoop clientName clientId channel

chatLoop :: String -> String -> Chan Message -> IO ()
chatLoop clientName clientId channel = do
    putStr (clientName ++ ": ")
    hFlush stdout
    userInput <- getLine
    putStrLn ""

    if userInput == "sair"
        then do
            putStrLn $ "\n****************************** Chat encerrado pelo cliente! ***********************************"
            return ()
        else do
            let clientMessage = Message (clientName ++ " (" ++ clientId ++ ")") userInput
            writeChan channel clientMessage
            
            putStr "\n                                                          Atendente: "
            hFlush stdout
            attendantResponse <- getLine
            putStrLn ""
            
            let attendantMessage = Message "Atendente" attendantResponse
            writeChan channel attendantMessage
            
            chatLoop clientName clientId channel