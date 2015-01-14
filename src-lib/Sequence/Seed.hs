module Sequence.Seed
( Seed (..)
, newSeed
) where

import System.Random (newStdGen, random)

newtype Seed = Seed Int

instance Show Seed where
    show (Seed x) = show x

instance Eq Seed where
    (Seed l) == (Seed r) = l == r

newSeed :: IO Seed
newSeed = do
    rng <- newStdGen
    let (seed, _) = random rng    
    return $ Seed seed