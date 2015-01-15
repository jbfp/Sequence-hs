module Sequence.Lobby
( Lobby (..)
, LobbyError
, joinLobby
, isFull
) where

import Data.UUID (UUID)
import Sequence.Capacity
import Sequence.Player (Player)

data Lobby = Lobby
    { lobbyId :: UUID
    , capacity :: Capacity
    , players :: [Player] }
    deriving (Show)

instance Eq Lobby where
    l == r = (lobbyId l) == (lobbyId r)

data LobbyError = PlayerAlreadyExists
                | LobbyIsFull
                | LobbyIsNotFull
                deriving (Show)

isFull :: Lobby -> Bool
isFull (Lobby _ cap ps) = (length ps) == (numPlayers cap)

confirm :: (a -> Bool) -> a -> e -> Either e ()
confirm f x e
    | f x = Right ()
    | otherwise = Left e

joinLobby :: Player -> Lobby -> Either LobbyError Lobby
joinLobby player lobby@(Lobby _ cap ps) = do    
    let mp = (numTeams cap) * (numPlayersPerTeam cap)
    confirm (notFull mp) ps LobbyIsFull
    confirm (playerDoesNotExist player) ps PlayerAlreadyExists
    return lobby { players = player : (players lobby) }

playerDoesNotExist :: Eq a => a -> [a] -> Bool
playerDoesNotExist = notElem

notFull :: Int -> [Player] -> Bool
notFull mp ps = mp > length ps