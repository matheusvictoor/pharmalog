module Services.ClientService where

import Models.Client
import Models.Sale
import Data.List (find, deleteBy)


createClient :: Client -> [Client] -> [Client]
createClient newClient clients = newClient : clients


getAllClients :: [Client] -> [Client]
getAllClients = id

getClientByCpf :: String -> [Client] -> Maybe Client
getClientByCpf searchCpf = find (\client -> cpf client == searchCpf)


updateClient :: String -> Client -> [Client] -> [Client]
updateClient searchCpf updatedClient = map updateIfFound
  where
    updateIfFound client
      | cpf client == searchCpf = updatedClient
      | otherwise = client


deleteClient :: String -> [Client] -> [Client]
deleteClient searchCpf = deleteBy (\client _ -> cpf client == searchCpf) undefined


addSaleToClient :: String -> Sale -> [Client] -> [Client]
addSaleToClient searchCpf newSale = map updateIfFound
  where
    updateIfFound client
      | cpf client == searchCpf = client { sales = newSale : sales client }
      | otherwise = client
