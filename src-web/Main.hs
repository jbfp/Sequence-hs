{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sequence.Api where

import Control.Concurrent
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
    -- Match all routes, routes that allows anyone should be above this line.
    matchAny (function (\_ -> Just [])) $ authorize key users 
    
    get "/lobbies" $ getLobbies lobbyList
    post "/lobbies/:lobbyId" $ createLobby lobbyList
    post "/lobbies/:lobbyId/players" $ postJoinLobby lobbyList gameList
    
    get "/games" $ getGames gameList
    get "/games/:gameId" $ getGame gameList    

getLobbies :: LobbyList -> ActionResult
getLobbies lobbyList = do
    lobbies <- liftIO $ withMVar lobbyList return
    json lobbies

createLobby :: LobbyList -> ActionResult
createLobby lobbyList = do
    -- Get lobby ID from URL param.
    lId <- param "lobbyId"

    -- Get user ID from authorization header.    
    playerId <- getUserId
    let human = Human playerId

    -- Validate capacity from request body.
    requestBodyJson <- jsonData
    let nt = numTeams requestBodyJson
    let nppt = numPlayersPerTeam requestBodyJson
    validatedCapacity <-
        case mkCapacity nt nppt of
            Left err -> raise $ BadRequest $ show err
            Right c -> return c

    let lobby = Lobby { lobbyId = lId, capacity = validatedCapacity, players = []}

    case human `joinLobby` lobby of
        Left err' -> do status status500; json $ String $ T.pack $ show err'
        Right lobby' -> do
            liftIO $ modifyMVar_ lobbyList (\lobbies -> return $ lobby' : lobbies)
            status status201
            json lobby'

postJoinLobby :: LobbyList -> GameList -> ActionResult
postJoinLobby lobbyList gameList = do
    -- Get user ID from authorization header.    
    playerId <- getUserId
    let human = Human playerId
    
    -- Get lobby ID from URL param.
    lId <- param "lobbyId"

    -- Get the lobby by ID.
    lobby <- liftIO $ modifyMVar lobbyList (\lobbies -> do
        let maybeIndex = findIndex (\lobby -> lobbyId lobby == lId) lobbies
        case maybeIndex of
            Nothing -> return (lobbies, Left NotFound)
            Just idx -> do
                let (left, lobby : right) = splitAt idx lobbies

                case human `joinLobby` lobby of
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

getGames :: GameList -> ActionResult
getGames gameList = do
    games <- liftIO $ withMVar gameList return
    json games

getGame :: GameList -> ActionResult
getGame gameList = do
    gId <- param "gameId"
    games <- liftIO $ withMVar gameList return
    
    case find (\g -> gameId g == gId) games of
        Nothing -> raise NotFound
        Just game -> json game
        
getUserId :: ActionT ErrorResult IO UUID
getUserId = do
    authorization <- header "Authorization"
    let val = fromString $ TL.unpack $ fromMaybe "" authorization
    case val of
        Nothing -> raise NotFound
        Just pId -> return pId 