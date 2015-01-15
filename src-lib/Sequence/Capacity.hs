module Sequence.Capacity
( Capacity
, numTeams
, numPlayersPerTeam
, CapacityError (..)
, mkCapacity
, numPlayers
) where

import Control.Applicative ((<$>), (<*>))

data Capacity = CapacityT
    { numTeams :: Int
    , numPlayersPerTeam :: Int }
    deriving (Show)

data CapacityError = InvalidNumTeams
                   | InvalidNumPlayersPerTeam
                   deriving (Show)

numPlayers :: Capacity -> Int
numPlayers (CapacityT nt nppt) = nt * nppt

mkCapacity :: Int -> Int -> Either CapacityError Capacity
mkCapacity nt nppt = CapacityT
    <$> validate isValidNumTeams nt InvalidNumTeams
    <*> validate isValidNumPlayersPerTeam nppt InvalidNumPlayersPerTeam

validate :: (a -> Bool) -> a -> e -> Either e a
validate f x e
    | f x = Right x
    | otherwise = Left e 

isValidNumTeams :: Int -> Bool
isValidNumTeams n = case n of
    2 -> True
    3 -> True
    _ -> False

isValidNumPlayersPerTeam :: Int -> Bool
isValidNumPlayersPerTeam n = case n of
    1 -> True
    2 -> True
    _ -> False