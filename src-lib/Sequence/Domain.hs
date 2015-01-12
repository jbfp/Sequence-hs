{-# LANGUAGE TypeFamilies #-}

module Sequence.Domain where

import Control.Monad.State
import Data.Maybe
import Sequence.Aggregate
import Sequence.Board
import Sequence.Cards
import Sequence.Utils (removeFirst, mapi, pop, replace')
import System.Random (mkStdGen)

newtype Seed = Seed Int deriving (Show, Read, Eq)
newtype GameId = GameId Int deriving (Show, Read, Eq)
data Team = Red | Green | Blue deriving (Show, Read, Eq, Enum)
data Capacity = Capacity { numTeams :: Int, numPlayersPerTeam :: Int } deriving (Show, Read)
data TileChange = TileChange Tile Int Int Card deriving (Show, Eq)
data Player = Human Int
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
                               , team :: Int }
                               deriving (Show)

data Game = Zero
          | Open GameId [Player] Capacity
          | Playing Deck [PlayerState] Board
          | Over [Player] -- TODO: stats, antal sekvenser for hvert hold osv.

instance Show Game where
  show Zero = "Zero"
  show (Open gameId players _) = "Open - Id: " ++ show gameId ++ ", Players: " ++ show players  
  show (Playing deck (p:_) _) = "Playing - Cards left in deck: " ++ show (length deck) ++ ", " ++
                                "Current player: " ++ show p ++ ", " ++
                                "Hand: " ++ show (hand p)

instance Aggregate Game where
  data Error Game = InvalidNumberOfTeams
                  | InvalidNumberOfPlayersPerTeam
                  | GameHasAlreadyBeenCreated
                  | GameIsNotOpen
                  | GameIsNotOngoing
                  | PlayerAlreadyExists
                  | GameIsFull
                  | GameIsNotFull
                  | ItIsNotYourTurn
                  | DoesNotHaveCard
                  | CardDoesNotMatchTile
                  | TileIsNotEmpty
                  | TileIsEmpty
                  | TileIsPartOfSequence
                  deriving (Show)

  data Event Game = Created GameId Capacity
                  | Joined Player
                  | Started Seed
                  | MovePerformed Int Int Card
                  | Ended Int
                  deriving (Show)

  data Command Game = Create GameId Capacity
                    | Join Player
                    | Start Seed
                    | PerformMove Player Int Int Card
                    deriving (Show)

  -- todo: make pretty :3
  -- Create game.
  execute game (Create gameId capacity) =
    case game of
      Zero -> do
        confirm (isValidNumTeams nt) InvalidNumberOfTeams
        confirm (isValidNumPlayersPerTeam nppt) InvalidNumberOfPlayersPerTeam
        return [Created gameId capacity]        
        where nt = numTeams capacity
              nppt = numPlayersPerTeam capacity
      _ -> Left GameHasAlreadyBeenCreated

  -- Join game.
  execute game (Join p) =
    case game of
      Open _ players capacity -> do
        let mp = maxPlayers capacity
        confirm (notFull mp players) GameIsFull
        confirm (playerDoesNotExist p players) PlayerAlreadyExists
        return $ (Joined p) : (if (isFull mp (p : players)) then [Started $ Seed 42] else [])
      _ -> Left GameIsNotOpen

  -- Start game.
  execute game (Start seed) =
    case game of
      Open _ players capacity -> do
        confirm (isFull (maxPlayers capacity) players) GameIsNotFull
        return [Started seed]
      _ -> Left GameIsNotOpen

  -- Perform move.
  execute game (PerformMove p row column card@(Card suit rank)) =
    case game of
      Playing _ (currentPlayer : players) board -> do        
        confirm (isPlayersTurn players p) ItIsNotYourTurn
        confirm (hasCard card currentHand) DoesNotHaveCard
        
        case (suit, rank) of
          (Spades, Jack)         -> do confirmTileIsOccuppied; confirmTileIsNotPartOfSequence
          (Clubs, Jack)          -> do confirmTileIsOccuppied; confirmTileIsNotPartOfSequence
          (Hearts, Jack)         -> confirmTileIsEmpty
          (Diamonds, Jack)       -> confirmTileIsEmpty
          (_,                 _) -> do confirmTileIsEmpty; confirm (matchesTile card row column) CardDoesNotMatchTile
        
        let playerTeam = team currentPlayer
        let newBoard = replace' row column (Just playerTeam, (row, column)) board
        return $ (MovePerformed row column card) : (if isWinner newBoard playerTeam then [Ended playerTeam] else [])
        where currentHand = hand currentPlayer
              confirmTileIsOccuppied = confirm (True) TileIsEmpty
              confirmTileIsNotPartOfSequence = confirm (isNotPartOfSequence board row column) TileIsPartOfSequence
              confirmTileIsEmpty = confirm (tileIsEmpty board row column) TileIsNotEmpty
      _ -> Left GameIsNotOngoing

  apply Zero (Created gameId capacity) =
    Open gameId [] capacity

  apply (Open gameId ps capacity) (Joined p) =
    Open gameId (p : ps) capacity

  apply (Open _ players capacity) (Started (Seed seed)) =
    Playing deck mappedPlayers mkBoard
      where rng = mkStdGen seed
            shuffledDeck = makeShuffledDeck rng
            numPlayers = length players
            numCardsPerPlayer = getNumCards numPlayers
            nppt = numPlayersPerTeam capacity
            (hands, deck) = runState (dealHands numPlayers numCardsPerPlayer) shuffledDeck
            mappedPlayers = mapi (\i p -> PlayerState { player = p, team = (i `mod` nppt), hand = (hands !! i) }) players
  
  apply (Playing deck (currentPlayer:players) board) (MovePerformed row column card) =
    Playing rest (players ++ [modifiedPlayer]) board'
      where currentHand = removeFirst card $ hand currentPlayer -- remove the card the player has used.
            (newCard, rest) = pop deck -- get a new card for him/her, if there is one. TODO: tomt deck.
            modifiedPlayer = currentPlayer { hand = currentHand ++ (maybeToList newCard) } -- maybeToList turns Nothing into [] and Just a into [a].
            value = (Just $ team currentPlayer, (row, column))
            board' = replace' row column value board

  apply (Playing _ players _) (Ended winnerTeam) = 
    Over $ fmap (\p -> player p) $ filter (\p -> team p == winnerTeam) players

  apply _ _ = error "Cannot apply event to state."

  zero = Zero

replay :: (Aggregate a) => [Event a] -> a
replay = foldl apply zero

maxPlayers :: Capacity -> Int
maxPlayers capacity = (numTeams capacity) * (numPlayersPerTeam capacity)

-- Validation functions.
confirm :: Bool -> e -> Either e ()
confirm True _ = Right ()
confirm False err = Left err

isValidNumTeams :: Int -> Bool
isValidNumTeams n =
  case n of
    2 -> True
    3 -> True
    _ -> False

isValidNumPlayersPerTeam :: Int -> Bool
isValidNumPlayersPerTeam n =
  case n of
    1 -> True
    2 -> True
    _ -> False

playerDoesNotExist :: Eq a => a -> [a] -> Bool
playerDoesNotExist p ps = p `notElem` ps

notFull :: Int -> [Player] -> Bool
notFull mp players = mp > length players

isFull :: Int -> [Player] -> Bool
isFull mp players = mp == length players

isPlayersTurn :: [PlayerState] -> Player -> Bool
isPlayersTurn [] _ = False
isPlayersTurn (x:_) p = (player x) == p

hasCard :: Card -> [Card] -> Bool
hasCard card cards = card `elem` cards

tileIsEmpty :: Board -> Int -> Int -> Bool
tileIsEmpty board row column = fst (getTile board row column) == Nothing

isNotPartOfSequence :: Board -> Int -> Int -> Bool
isNotPartOfSequence board row column = any ((==) coordinate) coordinates
  -- Get ALL sequences from all possible sequences, then concat, then get throw away the team whose sequence it is,
  -- then concat those sequences. Flatten the sequences so we only get a list of tiles, that are part of some sequence,
  -- and throw away the value of the tile because we don't care.
  where sequences = concat $ fmap getAllSequences $ possibleSequences board        
        coordinates = fmap (snd) $ concat $ concat $ fmap (snd) sequences
        coordinate = (row, column)