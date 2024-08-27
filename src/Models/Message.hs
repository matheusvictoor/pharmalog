module Models.Message where

data Message = Message {
  sender :: String,
  content :: String
} deriving (Show)