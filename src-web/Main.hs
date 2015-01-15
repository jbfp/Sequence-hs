{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sequence.Api where

import Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar_)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Data.Aeson hiding (json)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.UUID (fromString, nil, UUID)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Sequence.Api.Json ()
import Sequence.Capacity (mkCapacity)
import Sequence.Game hiding (players)
import Sequence.Lobby
import Sequence.Player
import Sequence.Seed
import Web.Scotty

type LobbyList = MVar [Lobby]
type GameList = MVar [Game]

main :: IO ()
main = do
    lobbies <- newMVar []
    games <- newMVar []
    scotty 3000 $ app lobbies games

app :: LobbyList -> GameList -> ScottyM ()
app lobbyList gameList = do
    middleware logStdoutDev

    get "/lobbies" $ do        
        lobbies <- liftIO $ withMVar lobbyList (return)        
        json lobbies

    post "/lobbies/:lobbyId" $ do
        -- Get lobby ID from URL param.        
        lId <- param "lobbyId"

        -- Get user ID from authorization header.
        req <- request
        let reqHeaders = requestHeaders req
        let authorization = lookup "Authorization" reqHeaders

        case authorization of
            Nothing -> do status status401
            Just value -> do
                let playerId = fromString $ T.unpack $ TE.decodeUtf8 value

                case playerId of
                    Nothing -> do status status401
                    Just pId -> do
                        let human = Human pId

                        -- Validate capacity from request body.        
                        requestBodyJson <- jsonData
                        let nt = numTeams requestBodyJson
                        let nppt = numPlayersPerTeam requestBodyJson
                        let validatedCapacity = mkCapacity nt nppt

                        case validatedCapacity of
                            Left err -> do status status400; json $ String $ T.pack $ show err
                            Right c -> do
                                let lobby = Lobby { lobbyId = lId, capacity = c, players = []}

                                case human `joinLobby` lobby of
                                    Left err' -> do status status500; json $ String $ T.pack $ show err'
                                    Right lobby' -> do
                                        liftIO $ modifyMVar_ lobbyList (\lobbies -> return $ lobby' : lobbies)
                                        status status201
                                        json lobby'
    
    get "/games" $ do        
        games <- liftIO $ withMVar gameList (return)        
        json games

    get "/games/:gameId" $ do
        gId <- param "gameId"
        seed <- liftIO $ newSeed
        json $ mkGame gId [] seed

instance Parsable UUID where
    parseParam t = maybeToEither "Could not parse UUID." $ fromString $ TL.unpack t
        where maybeToEither _ (Just x) = Right x
              maybeToEither e (Nothing) = Left e

data CreateGameRequest = CreateGameRequest { numTeams :: Int, numPlayersPerTeam :: Int } deriving (Generic)
instance FromJSON CreateGameRequest