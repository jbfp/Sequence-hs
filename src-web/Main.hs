{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sequence.Api where

import Control.Concurrent
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Data.Aeson hiding (json)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.UUID (fromString, UUID)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Sequence.Api.Controllers.Auth
import Sequence.Api.Error
import Sequence.Api.Json ()
import Sequence.Api.Models.User
import Sequence.Capacity (mkCapacity)
import Sequence.Game hiding (players)
import Sequence.Lobby
import Sequence.Player
import Sequence.Seed
import System.Environment
import Web.JWT hiding (header)
import Web.Scotty.Trans

type LobbyList = MVar [Lobby]
type GameList = MVar [Game]
type ActionResult = ActionT ErrorResult IO ()

instance Parsable UUID where
    parseParam t = maybeToEither "Could not parse UUID." $ fromString $ TL.unpack t
        where maybeToEither _ (Just x) = Right x
              maybeToEither e (Nothing) = Left e

data CreateGameRequest = CreateGameRequest { numTeams :: Int, numPlayersPerTeam :: Int } deriving (Generic)
instance FromJSON CreateGameRequest

main :: IO ()
main = do
    lobbies <- newMVar []
    games <- newMVar []
    users <- newMVar []
    key <- fmap (secret . T.pack) (getEnv "SequenceSecret")
    
    scottyT 3000 id id $ do
        middleware logStdoutDev
        defaultHandler handleErrorResult
        auth key users        
        app key lobbies games users

app :: Secret -> LobbyList -> GameList -> UserList -> ScottyT ErrorResult IO ()
app key lobbyList gameList users = do
    let authorizer = authorize key users
    
    get "/lobbies" $ getLobbies lobbyList authorizer
    post "/lobbies/:lobbyId" $ createLobby lobbyList authorizer
    post "/lobbies/:lobbyId/players" $ postJoinLobby lobbyList gameList authorizer
    
    get "/games" $ getGames gameList authorizer
    get "/games/:gameId" $ getGame gameList authorizer

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
                let game = mkGame lId (players l) seed
                liftIO $ modifyMVar_ gameList (\games -> return $ game : games)
                json game
            else json l

getGames :: GameList -> Authorizer -> ActionResult
getGames gameList authorizer = do
    _ <- authorizer
    games <- liftIO $ withMVar gameList return
    json games

getGame :: GameList -> Authorizer -> ActionResult
getGame gameList authorizer = do
    _ <- authorizer
    gId <- param "gameId"
    games <- liftIO $ withMVar gameList return
    
    case find (\g -> gameId g == gId) games of
        Nothing -> raise NotFound
        Just game -> json game
        
getPlayerFromAuth :: Authorizer -> ActionT ErrorResult IO Player
getPlayerFromAuth = liftM (Human . unUserId)    