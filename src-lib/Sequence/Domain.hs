module Sequence.Domain where

import Data.UUID (UUID)
import Sequence.Cards (Card)

newtype Seed = Seed Int
type Row = Int
type Column = Int

instance Show Seed where
  show (Seed x) = show x

instance Eq Seed where
  (Seed l) == (Seed r) = l == r

data Team = Red | Green | Blue deriving (Show, Read, Eq, Enum)
data Capacity = Capacity { numTeams :: Int, numPlayersPerTeam :: Int } deriving (Show, Read)
data Player = Human UUID
            | Bot String

instance Eq Player where
  (Human id1) == (Human id2) = id1 == id2
  (Bot name1) == (Bot name2) = name1 == name2
  _ == _ = False

instance Show Player where
    show (Human humanId) = show humanId
    show (Bot value) = show value

data PlayerState = PlayerState { player :: Player
                               , hand :: [Card]
                               , team :: Team }
                               deriving (Show)