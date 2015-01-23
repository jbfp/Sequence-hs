{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sequence.Api.Controllers.Lobby where

import Control.Concurrent
import Control.Monad.Trans (liftIO)
import Data.Aeson hiding (json)
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Network.HTTP.Types
import Sequence.Api.Controllers.Auth
import Sequence.Api.Error
import Sequence.Api.Json ()
import Sequence.Api.Models.Events
import Sequence.Api.Models.User
import Sequence.Api.Parsables ()
import Sequence.Capacity (mkCapacity)
import Sequence.Game hiding (players, version)
import Sequence.Lobby
import Sequence.Player
import Sequence.Seed
import Web.JWT
import Web.Scotty.Trans

type LobbyList = MVar [Lobby]
type ActionResult = ActionT ErrorResult IO ()

data CreateGameRequest = CreateGameRequest { numTeams :: Int, numPlayersPerTeam :: Int } deriving (Generic)
instance FromJSON CreateGameRequest

lobby :: Secret -> LobbyList -> GameList -> UserList -> ScottyT ErrorResult IO ()
lobby key lobbyList gameList users = do
    let authorizer = authorize key users
    
    get "/lobbies" $ getLobbies lobbyList authorizer
    post "/lobbies/:lobbyId" $ createLobby lobbyList authorizer
    post "/lobbies/:lobbyId/players" $ postJoinLobby lobbyList gameList authorizer
    
getLobbies :: LobbyList -> Authorizer -> ActionResult
getLobbies lobbyList authorizer = do
    _ <- authorizer    
    lobbies <- liftIO $ withMVar lobbyList return
    json lobbies

createLobby :: LobbyList -> Authorizer -> ActionResult
createLobby lobbyList authorizer = do
    -- Get lobby ID from URL param.
    lId <- param "lobbyId"

    -- Get user ID from authorization header.
    player <- getPlayerFromAuth authorizer

    -- Validate capacity from request body.
    requestBodyJson <- jsonData
    let nt = numTeams requestBodyJson
    let nppt = numPlayersPerTeam requestBodyJson
    validatedCapacity <-
        case mkCapacity nt nppt of
            Left err -> raise $ BadRequest $ show err
            Right c -> return c

    let lobby = Lobby { lobbyId = lId, capacity = validatedCapacity, players = []}

    case player `joinLobby` lobby of
        Left err' -> do status status500; json $ String $ T.pack $ show err'
        Right lobby' -> do
            liftIO $ modifyMVar_ lobbyList (\lobbies -> return $ lobby' : lobbies)
            status status201
            json lobby'

postJoinLobby :: LobbyList -> GameList -> Authorizer -> ActionResult
postJoinLobby lobbyList gameList authorizer = do
    -- Get user ID from authorization header.
    player <- getPlayerFromAuth authorizer
    
    -- Get lobby ID from URL param.
    lId <- param "lobbyId"

    -- Get the lobby by ID.
    lobby <- liftIO $ modifyMVar lobbyList (\lobbies -> do
        let maybeIndex = findIndex (\lobby -> lobbyId lobby == lId) lobbies
        case maybeIndex of
            Nothing -> return (lobbies, Left NotFound)
            Just idx -> do
                let (left, lobby : right) = splitAt idx lobbies

                case player `joinLobby` lobby of
                    Left err -> return (lobbies, Left $ InternalServerError $ show err)
                    Right lobby' ->
                        return (if isFull lobby'
                                then (left ++ right, Right lobby') 
                                else (lobby' : (left ++ right), Right lobby')))

    case lobby of
        Left err -> raise err
        Right l ->
            -- Start game if full.
            if isFull l then do
                seed <- liftIO newSeed
                let either = start lId (players l) seed
                
                case either of
                    Left e -> raise $ InternalServerError $ show e 
                    Right gameEvent -> do
                        let wrapped = GameEventWrapper gameEvent lId 0
                        liftIO $ modifyMVar_ gameList (\games -> return $ wrapped : games)
                        json $ replay [gameEvent]
            else json l