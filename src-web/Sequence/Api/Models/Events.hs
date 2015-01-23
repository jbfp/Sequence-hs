module Sequence.Api.Models.Events where

import Control.Concurrent (MVar)
import Data.Function (on)
import Data.List (sortBy)
import Data.UUID (UUID)
import Sequence.Game (GameEvent)

data GameEventWrapper = GameEventWrapper
    { event :: GameEvent
    , aggregateId :: UUID
    , version :: Int }
  
sortByVersion :: [GameEventWrapper] -> [GameEventWrapper]
sortByVersion = sortBy (compare `on` version)

type GameList = MVar [GameEventWrapper]