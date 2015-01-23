{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sequence.Api.Json where

import Control.Applicative
import Data.Aeson ((.:), (.=), FromJSON, object, parseJSON, toJSON, ToJSON, Value (..))
import qualified Data.Text as T
import Data.UUID (UUID)
import Sequence.Capacity
import Sequence.Cards (Card (..))
import Sequence.Player
import qualified Sequence.Game as G
import qualified Sequence.Lobby as L

instance ToJSON UUID where
    toJSON uuid = String $ T.pack $ show uuid

-- TODO: Turn orphan instances into view models + mapping functions. Ugh.  

instance ToJSON Capacity where
    toJSON capacity = 
        object [ "numTeams"          .= numTeams capacity
               , "numPlayersPerTeam" .= numPlayersPerTeam capacity ]

instance ToJSON G.Game where
    toJSON game =
        object [ "id"      .= G.gameId game
               , "players" .= (mapToPlayerViewModel <$> G.players game)
               , "version" .= G.version game ]

mapToPlayerViewModel :: PlayerState -> PlayerViewModel
mapToPlayerViewModel (PlayerState p cards t) = case p of
    Human uuid -> PlayerViewModel (T.pack $ show uuid) t cards
    Bot name -> PlayerViewModel name t cards

data PlayerViewModel = PlayerViewModel
    { vmName :: T.Text
    , vmTeam :: Team
    , vmHand :: [Card] }

instance ToJSON PlayerViewModel where
    toJSON (PlayerViewModel n t h) =
        object [ "name" .= n
               , "team" .= t
               , "hand" .= h ]

instance ToJSON Team where
    toJSON t = String $ T.pack $ show t        

instance ToJSON PlayerState where
    toJSON ps =
        object [ "player" .= player ps
               , "hand"   .= hand ps
               , "team"   .= team ps ]

instance ToJSON Player where
    toJSON (Human uuid) =
        object [ "name" .= show uuid ]

    toJSON (Bot name) =
        object [ "name" .= name ]

instance ToJSON Card where
    toJSON (Card s r) =
        object [ "suit" .= show s
               , "rank" .= show r ]

instance ToJSON L.Lobby where
    toJSON lobby =
        object [ "id"       .= L.lobbyId lobby 
               , "capacity" .= L.capacity lobby 
               , "players"  .= L.players lobby ]