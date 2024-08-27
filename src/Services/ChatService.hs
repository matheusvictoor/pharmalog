module Services.ChatService (simuleChat) where

import Models.Message

simuleChat :: IO ()
simuleChat = do
  putStrLn "Bem vindo ao chat"