module Sequence.Game
( Game
, gameId
, deck
, players
, board
, mkGame
) where

import Control.Applicative ((<$>))
import Control.Monad.State (runState)
import Data.Maybe (isNothing)
import Data.UUID (UUID)
import Sequence.Board (Board, getAllSequences, getTile, mkBoard, possibleSequences)
import Sequence.Cards (Card (..), dealHands, Deck, getNumCards, makeShuffledDeck)
import Sequence.Player (hand, Player, PlayerState (..))
import Sequence.Seed (Seed (..))
import Sequence.Utils
import System.Random (mkStdGen)

data GameError = GameIsNotOngoing
               | ItIsNotYourTurn
               | DoesNotHaveCard
               | CardDoesNotMatchTile
               | TileIsNotEmpty
               | TileIsEmpty
               | TileIsPartOfSequence
               deriving (Show)

data Game = GameT
    { gameId :: UUID  
    , deck :: Deck
    , players :: [PlayerState]
    , board :: Board }

mkGame :: UUID -> [Player] -> Seed -> Game
mkGame uuid ps (Seed seed) = GameT
    { gameId = uuid
    , deck = rest
    , players = mappedPlayers
    , board = mkBoard }
    where rng = mkStdGen seed
          shuffledDeck = makeShuffledDeck rng
          numPlayers = length ps
          numCardsPerPlayer = getNumCards numPlayers
          nppt = numPlayersToTeamSize numPlayers
          (hands, rest) = runState (dealHands numPlayers numCardsPerPlayer) shuffledDeck
          mappedPlayers = mapi (\i p -> PlayerState { player = p, team = toEnum (i `mod` nppt), hand = hands !! i }) ps           

numPlayersToTeamSize :: Int -> Int
numPlayersToTeamSize n = case n of
    2 -> 1
    3 -> 1
    4 -> 2
    6 -> 2
    _ -> error "Invalid number of players."

isPlayersTurn :: [PlayerState] -> Player -> Bool
isPlayersTurn [] _ = False
isPlayersTurn (x:_) p = player x == p

hasCard :: Card -> [Card] -> Bool
hasCard card cards = card `elem` cards

tileIsEmpty :: Board -> Int -> Int -> Bool
tileIsEmpty b row column = isNothing (fst (getTile b row column))

isNotPartOfSequence :: Board -> Int -> Int -> Bool
isNotPartOfSequence b row column = coordinate `elem` coordinates
    -- Get ALL sequences from all possible sequences, then concat, then get throw away the team whose sequence it is,
    -- then concat those sequences. Flatten the sequences so we only get a list of tiles, that are part of some sequence,
    -- and throw away the value of the tile because we don't care.
    where sequences = concat (getAllSequences <$> possibleSequences b)
          coordinates = fmap snd $ concat $ concat $ fmap snd sequences
          coordinate = (row, column)