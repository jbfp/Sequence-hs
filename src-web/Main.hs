{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sequence.Api where

import Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar)
import Control.Monad.Trans (liftIO)
import Data.Aeson ((.=), object, ToJSON, toJSON, Value(..))
import Data.Text as T (pack)
import Data.Text.Lazy (unpack)
import Data.UUID (fromString, UUID)
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Sequence.Aggregate (apply, Error, Event, execute)
import Sequence.Cards (Card (..))
import Sequence.Domain (Capacity (..), Player (..), Seed (..))
import Sequence.Game
import Web.Scotty

type GameList = MVar [Game]

app :: GameList -> ScottyM ()
app games = do
    middleware logStdoutDev

    get "/games" $ do
        result <- liftIO $ withMVar games (return)
        json result

    post "/games" $ do
        gameId <- liftIO $ nextRandom
        game <- liftIO $ modifyMVar games $ \gs -> return ((Open gameId [] (Capacity { numTeams = 2, numPlayersPerTeam = 1 })) : gs, (Open gameId [] (Capacity { numTeams = 2, numPlayersPerTeam = 1 })))
        json game

    get "/games/:gameId" $ do
        gameId <- param "gameId"
        json $ apply Zero $ Created gameId (Capacity { numTeams = 2, numPlayersPerTeam = 1 })

    post "/games/:gameId/players/:playerId" $ do
        gameId <- param "gameId"
        playerId <- param "playerId"        
        let event = execute (Open gameId [Bot "Bob"] (Capacity { numTeams = 2, numPlayersPerTeam = 1 })) $ Join $ Human playerId
        case event of
            Right a -> do status status200; json a
            Left e -> do status status400; json e

main :: IO ()
main = do
    games <- newMVar []
    scotty 3000 $ app games

instance Parsable UUID where
    parseParam t = maybeToEither "Could not parse UUID." $ fromString $ unpack t
        where maybeToEither _ (Just x) = Right x
              maybeToEither e (Nothing) = Left e

instance ToJSON UUID where
    toJSON uuid = String $ T.pack $ show uuid

-- TODO: Turn orphan instances into view models + mapping functions. Ugh.
instance ToJSON Capacity where
    toJSON capacity = 
        object [ "numTeams"          .= numTeams capacity
               , "numPlayersPerTeam" .= numPlayersPerTeam capacity ]

instance ToJSON Game where
    toJSON Zero = object []

    toJSON (Open gameId players capacity) =
        object [ "id"       .= show gameId 
               , "players"  .= show players
               , "capacity" .= capacity ]

instance ToJSON Player where
    toJSON (Human uuid) =
        object [ "type" .= String "human"
               , "id" .= uuid ]

    toJSON (Bot name) =
        object [ "type" .= String "bot"
               , "name" .= name ]

instance ToJSON Card where
    toJSON (Card s r) =
        object [ "suit" .= show s
               , "rank" .= show r ]

instance ToJSON (Event Game) where
    toJSON (Created uuid capacity) =
        object [ "id"       .= uuid
               , "capacity" .= capacity
               , "type"     .= String "created" ]

    toJSON (Joined p) = 
        object [ "player" .= p
               , "type"   .= String "joined" ]

    toJSON (Started (Seed seed)) =
        object [ "seed" .= seed
               , "type" .= String "started" ]

    toJSON (MovePerformed row column card) =
        object [ "row"    .= row
               , "column" .= column
               , "card"   .= card
               , "type"   .= String "moveperformed" ]

    toJSON (Ended winnerTeam) =
        object [ "winner" .= show winnerTeam
               , "type"   .= String "ended" ]

instance ToJSON (Error Game) where
    toJSON x = object [ "error" .= show x ]