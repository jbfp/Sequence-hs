{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sequence.Api where

import Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar_)
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON)
import Data.List (groupBy)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.UUID (fromString, UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Sequence.Aggregate (apply, execute, executeIO, zero)
import Sequence.Api.EventData
import Sequence.Domain
import Sequence.Game
import Web.Scotty

type EventList = MVar [EventData Game]

app :: EventList -> ScottyM ()
app eventList = do
    middleware logStdoutDev

    get "/games" $ do
        result <- liftIO $ withMVar eventList (return)
        let grouped = groupBy (\l r -> aggregateId l == aggregateId r) result
        let games = fmap (replay . fmap (eventData)) grouped
        json games

    post "/games" $ (do
            req <- jsonData
    
            gameId <- liftIO $ nextRandom -- Generate UUID for the new game.
            let cap = capacity req
            let command = Create gameId cap
            let eitherGame = execute zero command
    
            case eitherGame of
              Left err -> do status status400; json err
              Right events -> do
                wrappedEvents <- mapM (\event -> return EventData { eventData = event, aggregateId = gameId, version = 1 }) events
                liftIO $ modifyMVar_ eventList (\es -> return $ wrappedEvents ++ es)                        
                json events ) `rescue` \e -> do status (Status { statusCode = 400, statusMessage = T.encodeUtf8 $ TL.toStrict $ e } )                    

    get "/games/:gameId" $ do
        gameId <- param "gameId"
        json $ apply Zero $ Created gameId (Capacity { numTeams = 2, numPlayersPerTeam = 1 })

    post "/games/:gameId/players/:playerId" $ do
        gameId <- param "gameId"
        let game = (Open gameId [Bot "Bob"] (Capacity { numTeams = 2, numPlayersPerTeam = 1 }) 1)

        playerId <- param "playerId"
        let human = Human playerId

        events <- liftIO $ executeIO game $ Join human
        case events of
            Right a -> do status status200; json a
            Left e -> do status status400; json e

main :: IO ()
main = do
    events <- newMVar []
    scotty 3000 $ app events

instance Parsable UUID where
    parseParam t = maybeToEither "Could not parse UUID." $ fromString $ TL.unpack t
        where maybeToEither _ (Just x) = Right x
              maybeToEither e (Nothing) = Left e

data CreateGameRequest = CreateGameRequest { capacity :: Capacity } deriving (Generic)
instance FromJSON CreateGameRequest