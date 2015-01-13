module Sequence.Matrix
( Matrix
, diagonals
) where

import Data.List (tails, transpose)

type Matrix a = [[a]]

diagonal :: Matrix a -> [a]
diagonal m = zipWith (!!) m [0..]

diagonals :: Matrix a -> [[a]]
diagonals matrix =
	diagonalsNW matrix ++ diagonalsNW (fmap reverse matrix)
	where tails' = tail . tails
  	      diagonalsNW m = fmap diagonal ([m] ++ tails' m ++ tails' (transpose m))	