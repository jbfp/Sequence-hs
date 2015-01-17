module Sequence.Utils
( cartesian
, double
, removeFirst
, mapi
, pop
, replace
, replace'
) where

-- Utility functions for working with lists.
cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = [(x, y) | x <- xs, y <- ys]

double :: [a] -> [a]
double xs = xs ++ xs

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys)
  | x == y    = ys
  | otherwise = y : removeFirst x ys

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi _       []   = []
mapi mapping list = zipWith mapping [0..] list

pop :: [a] -> (Maybe a, [a])
pop []     = (Nothing, [])
pop (x:xs) = (Just x, xs)

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i + 1) xs

replace' :: Int -> Int -> a -> [[a]] -> [[a]]
replace' row column element matrix = replace row (replace column element (matrix !! column)) matrix