{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sequence.Api where

import Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar_)
import Control.Monad.Trans (liftIO)
import Data.Aeson hiding (json)
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
import Sequence.Seed
import Web.Scotty

type LobbyList = MVar [Lobby]
type GameList = MVar [Game]

app :: LobbyList -> GameList -> ScottyM ()
app lobbyList gameList = do
    middleware logStdoutDev

    get "/lobbies" $ do        
        lobbies <- liftIO $ withMVar lobbyList (return)        
        json lobbies

    post "/lobbies/:lobbyId" $ do
        lId <- param "lobbyId"
        req <- jsonData

        -- Validate capacity.
        let nt = numTeams req
        let nppt = numPlayersPerTeam req
        let validatedCapacity = mkCapacity nt nppt

        case validatedCapacity of
            Left err -> do status status400; json $ String $ T.pack $ show err
            Right c -> do
                let lobby = Lobby { lobbyId = lId, capacity = c, players = []}
                liftIO $ modifyMVar_ lobbyList (\lobbies -> return $ lobby : lobbies)
                status status201
                json lobby
    
    get "/games" $ do        
        games <- liftIO $ withMVar gameList (return)        
        json games

    get "/games/:gameId" $ do
        gId <- param "gameId"
        seed <- liftIO $ newSeed
        json $ mkGame gId [] seed

main :: IO ()
main = do
    lobbies <- newMVar []
    games <- newMVar []
    scotty 3000 $ app lobbies games

instance Parsable UUID where
    parseParam t = maybeToEither "Could not parse UUID." $ fromString $ TL.unpack t
        where maybeToEither _ (Just x) = Right x
              maybeToEither e (Nothing) = Left e

data CreateGameRequest = CreateGameRequest { numTeams :: Int, numPlayersPerTeam :: Int } deriving (Generic)
instance FromJSON CreateGameRequest