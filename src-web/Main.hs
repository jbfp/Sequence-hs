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
import Sequence.Api.Json ()
import Sequence.Capacity (mkCapacity)
import Sequence.Game hiding (players)
import Sequence.Lobby
import Sequence.Player
import Sequence.Seed
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

data ErrorResult = BadRequest String
                 | Unauthorized
                 | NotFound
                 | InternalServerError String
                 deriving (Show, Eq)

instance ScottyError ErrorResult where
    stringError = InternalServerError
    showError = TL.pack . show

handleErrorResult :: Monad m => ErrorResult -> ActionT ErrorResult m ()
handleErrorResult (BadRequest err) = do status status400; json err
handleErrorResult Unauthorized = status status401
handleErrorResult NotFound = status status404
handleErrorResult (InternalServerError err) = do status status500; json err

main :: IO ()
main = do
    lobbies <- newMVar []
    games <- newMVar []
    scottyT 3000 id id $ app lobbies games

app :: LobbyList -> GameList -> ScottyT ErrorResult IO ()
app lobbyList gameList = do
    middleware logStdoutDev
    defaultHandler handleErrorResult

    get "/lobbies" $ getLobbies lobbyList
    post "/lobbies/:lobbyId" $ createLobby lobbyList
    post "/lobbies/:lobbyId/players" $ postJoinLobby lobbyList gameList
    
    get "/games" $ getGames gameList
    get "/games/:gameId" $ getGame gameList

getLobbies :: LobbyList -> ActionResult
getLobbies lobbyList = do
    lobbies <- liftIO $ withMVar lobbyList (return)
    json lobbies

createLobby :: LobbyList -> ActionResult
createLobby lobbyList = do
    -- Get lobby ID from URL param.
    lId <- param "lobbyId"

    -- Get user ID from authorization header.    
    playerId <- do
        authorization <- header "Authorization"
        val <- return $ fromString $ TL.unpack $ fromMaybe "" $ authorization
        case val of
            Nothing -> raise Unauthorized
            Just pId -> return pId

    let human = Human playerId

    -- Validate capacity from request body.
    requestBodyJson <- jsonData
    let nt = numTeams requestBodyJson
    let nppt = numPlayersPerTeam requestBodyJson
    validatedCapacity <- do
        case (mkCapacity nt nppt) of
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
    playerId <- do
        authorization <- header "Authorization"
        val <- return $ fromString $ TL.unpack $ fromMaybe "" $ authorization
        case val of
            Nothing -> raise Unauthorized
            Just pId -> return pId

    let human = Human playerId
    
    -- Get lobby ID from URL param.
    lId <- param "lobbyId"

    -- Get the lobby by ID.
    lobby <- liftIO $ modifyMVar lobbyList (\lobbies -> do
        let maybeIndex = findIndex (\lobby -> (lobbyId lobby) == lId) lobbies
        case maybeIndex of
            Nothing -> return (lobbies, Left NotFound)
            Just idx -> do
                let (left, (lobby:right)) = splitAt idx lobbies

                case human `joinLobby` lobby of
                    Left err -> return (lobbies, Left $ InternalServerError $ show err)
                    Right lobby' -> do
                        if isFull lobby' then
                            return (left ++ right, Right lobby')
                        else
                            return (lobby' : (left ++ right), Right lobby'))

    case lobby of
        Left err -> raise err
        Right l -> do 
            -- Start game if full.
            if isFull l then do
                seed <- liftIO $ newSeed
                let game = mkGame lId (players l) seed
                liftIO $ modifyMVar_ gameList (\games -> return $ game : games)
                json game
            else json l

getGames :: GameList -> ActionResult
getGames gameList = do
    games <- liftIO $ withMVar gameList (return)
    json games

getGame :: GameList -> ActionResult
getGame gameList = do
    gId <- param "gameId"
    games <- liftIO $ withMVar gameList (return)
    
    case (find (\g -> (gameId g) == gId) games) of
        Nothing -> raise NotFound
        Just game -> json game