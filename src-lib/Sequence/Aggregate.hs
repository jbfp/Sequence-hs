{-# LANGUAGE TypeFamilies #-}

module Sequence.Aggregate
( Aggregate
, Error
, Command
, Event
, execute
, executeIO
, apply
, zero
) where

class Aggregate s where
  data Error s :: *
  data Command s :: *
  data Event s :: *

  execute :: s -> Command s -> Either (Error s) ([Event s])
  executeIO :: s -> Command s -> IO (Either (Error s) ([Event s]))
  apply :: s -> Event s -> s
  zero :: s