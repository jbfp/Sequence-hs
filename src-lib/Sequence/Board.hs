module Sequence.Board
( Tile
, Board
, Sequence
, mkBoard
, possibleSequences
, isWinner
, getTile
, getSequencesForTeam
, getAllSequences
, matchesTile
) where

import Data.List
import Data.Maybe
import Sequence.Cards
import Sequence.Matrix

type Tile = (Maybe Int, (Int, Int))
type Board = Matrix Tile
type Sequence = [Tile]

mkBoard :: Board
mkBoard = [[(Nothing, (r, c)) | c <- [0..9 :: Int]] | r <- [0..9 :: Int]] -- Cast to Int to avoid the compiler assuming we're using Integer, not sure if necessary.

possibleSequences :: Board -> [Sequence]
possibleSequences board = horizontal ++ vertical ++ diagonal
    where horizontal = board
          vertical = transpose board
          diagonal = diagonals board

isWinner :: Board -> Int -> Bool
isWinner board i =
    any (\l -> length (getSequencesForTeam i l) == 2) $ possibleSequences board

getTile :: Board -> Int -> Int -> Tile
getTile board row column = ((board !! row) !! column)

getSequence' :: Sequence -> [Tile] -> Maybe Int -> [Sequence]
getSequence' _ [] _     = []
getSequence' (acc@(x:_)) (list@(y@(ty, _):ys)) team
    | (length acc) == 5 =
        let rest = x : list -- We append the head of the sequence, since one tile can be shared.
            sequences = getSequence' [] rest team
        in  acc : sequences
    | ty == team        = getSequence' (y : acc) ys team
    | otherwise         = getSequence' [] ys team

getSequencesForTeam :: Int -> [Tile] -> [Sequence]
getSequencesForTeam team tiles = getSequence' [] tiles (Just team)

getAllSequences :: [Tile] -> [(Int, [Sequence])]
getAllSequences tiles = fmap (\t -> (t, getSequencesForTeam t tiles)) teams
    where teams = nub $ catMaybes $ fmap (fst) $ filter (\(team, _) -> isJust team) tiles -- nub removes duplicates, catMaybes throws out Nothings.

suits :: Matrix (Maybe Suit)
suits = [[Nothing,       Just Diamonds, Just Diamonds, Just Diamonds, Just Diamonds, Just Diamonds, Just Diamonds, Just Diamonds, Just Diamonds, Nothing],
         [Just Diamonds, Just Hearts,   Just Hearts,   Just Spades,   Just Spades,   Just Spades,   Just Spades,   Just Spades,   Just Spades,   Just Clubs],
         [Just Diamonds, Just Hearts,   Just Diamonds, Just Diamonds, Just Clubs,    Just Clubs,    Just Clubs,    Just Clubs,    Just Spades,   Just Clubs],
         [Just Diamonds, Just Hearts,   Just Diamonds, Just Hearts,   Just Hearts,   Just Hearts,   Just Hearts,   Just Clubs,    Just Spades,   Just Clubs],
         [Just Diamonds, Just Hearts,   Just Diamonds, Just Hearts,   Just Hearts,   Just Hearts,   Just Hearts,   Just Clubs,    Just Spades,   Just Clubs],
         [Just Spades,   Just Hearts,   Just Diamonds, Just Hearts,   Just Hearts,   Just Hearts,   Just Hearts,   Just Clubs,    Just Spades,   Just Clubs],
         [Just Spades,   Just Hearts,   Just Diamonds, Just Clubs,    Just Clubs,    Just Clubs,    Just Clubs,    Just Clubs,    Just Spades,   Just Clubs],
         [Just Spades,   Just Hearts,   Just Diamonds, Just Diamonds, Just Diamonds, Just Diamonds, Just Diamonds, Just Diamonds, Just Spades,   Just Clubs],
         [Just Spades,   Just Hearts,   Just Hearts,   Just Hearts,   Just Hearts,   Just Clubs,    Just Clubs,    Just Clubs,    Just Clubs,    Just Clubs],
         [Nothing,       Just Spades,   Just Spades,   Just Spades,   Just Spades,   Just Spades,   Just Spades,   Just Spades,   Just Spades,   Nothing]]

ranks :: Matrix (Maybe Rank)
ranks = [[Nothing,    Just Six,   Just Seven, Just Eight, Just Nine,  Just Ten,  Just Queen, Just King,  Just Ace,   Nothing],
         [Just Five,  Just Three, Just Two,   Just Two,   Just Three, Just Four, Just Five,  Just Six,   Just Seven, Just Ace],
         [Just Four,  Just Four,  Just King,  Just Ace,   Just Ace,   Just King, Just Queen, Just Ten,   Just Eight, Just King],
         [Just Three, Just Five,  Just Queen, Just Queen, Just Ten,   Just Nine, Just Eight, Just Nine,  Just Nine,  Just Queen],
         [Just Two,   Just Six,   Just Ten,   Just King,  Just Three, Just Two,  Just Seven, Just Eight, Just Ten,   Just Ten],
         [Just Ace,   Just Seven, Just Nine,  Just Ace,   Just Four,  Just Five, Just Six,   Just Seven, Just Queen, Just Nine],
         [Just King,  Just Eight, Just Eight, Just Two,   Just Three, Just Four, Just Five,  Just Six,   Just King,  Just Eight],
         [Just Queen, Just Nine,  Just Seven, Just Six,   Just Five,  Just Four, Just Three, Just Two,   Just Ace,   Just Seven],
         [Just Ten,   Just Ten,   Just Queen, Just King,  Just Ace,   Just Two,  Just Three, Just Four,  Just Five,  Just Six],
         [Nothing,    Just Nine,  Just Eight, Just Seven, Just Six,   Just Five, Just Four,  Just Three, Just Two,   Nothing]]

matchesTile :: Card -> Int -> Int -> Bool
matchesTile card row column =
  case (suit, rank) of
    (Just s, Just r) -> (Card s r) == card
    (_     , _     ) -> False
  where suit = (suits !! row) !! column
        rank = (ranks !! row) !! column