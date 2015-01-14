module Sequence.Player
( Team
, Player (..)
, PlayerState (..)
) where

import Data.Text (Text)
import Data.UUID (UUID)
import Sequence.Cards (Card)

data Team = Red
		  | Green
		  | Blue
		  deriving (Show, Read, Eq, Enum)

data Player = Human UUID
            | Bot Text

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