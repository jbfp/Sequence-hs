module Sequence.Cards
( Suit (..)
, Rank (..)
, Card (..)
, Deck
, Hand
, getNumCards
, makeShuffledDeck
, dealHands
) where

import Control.Monad.State
import Sequence.Utils
import System.Random (RandomGen)
import System.Random.Shuffle

data Suit = Spades | Hearts | Clubs | Diamonds
          deriving (Read, Enum, Eq, Ord)

instance Show Suit where
    show s = case s of
        Spades   -> "♠"
        Hearts   -> "♥"
        Clubs    -> "♣"
        Diamonds -> "♦"

data Rank = Ace | Two | Three | Four | Five
          | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King
          deriving (Read, Enum, Eq, Ord)

instance Show Rank where
    show r = case r of
        Ace   -> "A"
        Two   -> "2"
        Three -> "3"
        Four  -> "4"
        Five  -> "5"
        Six   -> "6"
        Seven -> "7"
        Eight -> "8"
        Nine  -> "9"
        Ten   -> "10"
        Jack  -> "J"
        Queen -> "Q"
        King  -> "K"

data Card = Card Suit Rank deriving (Eq)
instance Show Card where
    show (Card s r) = show s ++ show r

type Deck = [Card]
type Hand = [Card]

makeDeck :: Deck
makeDeck = fmap (\(x, y) -> Card x y) cards :: Deck
    where suits = [Spades .. Diamonds]
          ranks = [Ace .. King]
          cards = double $ cartesian suits ranks

shuffleDeck :: RandomGen g => g -> Deck -> Deck
shuffleDeck g deck = shuffle' deck (length deck) g

-- | Shuffles a deck of 104 cards.
makeShuffledDeck :: RandomGen g => g -> Deck
makeShuffledDeck g = shuffleDeck g $ makeDeck

getNumCards :: Int -> Int
getNumCards n = case n of
    2 -> 7
    3 -> 6
    4 -> 5
    6 -> 4
    _ -> error "Invalid number of players."

-- https://hackage.haskell.org/package/HCard-0.0/docs/src/Data-HCard-Deck.html
dealHand :: Int -> State Deck Hand
dealHand qty = do
  deck <- get
  let (hand, rest) = splitAt qty deck
  put rest
  return hand

-- TODO: Remove qty parameter.
dealHands :: Int -> Int -> State Deck [Hand]
dealHands 0 _   = return []
dealHands _ 0   = error "Can't deal zero cards."
dealHands n qty = do
    deck <- get
    if length deck < qty then
        error "Not enough cards in deck."
    else do { hand <- dealHand qty
            ; next <- dealHands (n - 1) qty
            ; return $ hand : next }