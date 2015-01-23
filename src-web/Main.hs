{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sequence.Api where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Trans (liftIO)
import Data.Aeson hiding (json)
import Data.Function (on)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.UUID (fromString, UUID)
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Sequence.Api.Controllers.Auth
import Sequence.Api.Controllers.Lobby
import Sequence.Api.Error
import Sequence.Api.Json ()
import Sequence.Api.Models.Events
import Sequence.Api.Models.User
import Sequence.Api.Parsables ()
import Sequence.Capacity (mkCapacity)
import Sequence.Game hiding (players, version)
import Sequence.Player
import System.Environment
import Web.JWT hiding (header)
import Web.Scotty.Trans

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
        lobby key lobbies games users
        app key games users

app :: Secret -> GameList -> UserList -> ScottyT ErrorResult IO ()
app key gameList users = do
    let authorizer = authorize key users

    get "/games" $ getGames gameList authorizer
    get "/games/:gameId" $ getGame gameList authorizer

getGames :: GameList -> Authorizer -> ActionResult
getGames gameList authorizer = do
    _ <- authorizer
    gameEventWrappers <- liftIO $ withMVar gameList return
    let grouped = groupBy ((==) `on` aggregateId) gameEventWrappers
    let gameEvents = fmap (fmap event . sortByVersion) grouped    
    let games = fmap replay gameEvents
    json games

getGame :: GameList -> Authorizer -> ActionResult
getGame gameList authorizer = do
    _ <- authorizer
    gId <- param "gameId"
    gameEvents <- liftIO $ withMVar gameList return    
    let filtered = event <$> filter (\ g -> aggregateId g == gId) gameEvents
    let game = replay filtered
    
    if game == zero
    then raise NotFound
    else json game