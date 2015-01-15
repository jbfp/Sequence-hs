{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sequence.Api where

import Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar_)
import Control.Monad.Trans (liftIO)
import Data.Aeson hiding (json)
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

data ErrorResult = BadRequest String | Unauthorized | NotFound | InternalServerError String
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
    
    get "/games" $ getGames gameList
    get "/games/:gameId" $ getGame

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

getGames :: GameList -> ActionResult
getGames gameList = do
    games <- liftIO $ withMVar gameList (return)
    json games

getGame :: ActionResult
getGame = do
    gId <- param "gameId"
    seed <- liftIO $ newSeed
    json $ mkGame gId [] seed

instance Parsable UUID where
    parseParam t = maybeToEither "Could not parse UUID." $ fromString $ TL.unpack t
        where maybeToEither _ (Just x) = Right x
              maybeToEither e (Nothing) = Left e

data CreateGameRequest = CreateGameRequest { numTeams :: Int, numPlayersPerTeam :: Int } deriving (Generic)
instance FromJSON CreateGameRequest