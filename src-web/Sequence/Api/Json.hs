{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sequence.Api.Json where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.:), (.=), FromJSON, object, parseJSON, toJSON, ToJSON, Value(..))
import Data.Text as T (pack)
import Data.UUID (UUID)
import Sequence.Aggregate (Error, Event)
import Sequence.Cards (Card (..))
import Sequence.Domain (Capacity (..), Player (..))
import Sequence.Game

instance ToJSON UUID where
    toJSON uuid = String $ T.pack $ show uuid

    -- TODO: Turn orphan instances into view models + mapping functions. Ugh.
instance ToJSON Capacity where
    toJSON capacity = 
        object [ "numTeams"          .= numTeams capacity
               , "numPlayersPerTeam" .= numPlayersPerTeam capacity ]

instance FromJSON Capacity where
	parseJSON (Object c) =
		Capacity <$>
		(c .: "numTeams") <*>
		(c .: "numPlayersPerTeam")

instance ToJSON Game where
    toJSON Zero = object []

    toJSON (Open gameId players capacity version) =
        object [ "id"       .= show gameId 
               , "players"  .= show players
               , "capacity" .= capacity
               , "version"  .= version ]

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

    toJSON (Started seed) =
        object [ "seed" .= show seed
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