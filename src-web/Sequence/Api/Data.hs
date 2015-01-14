{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Sequence.Api.Data
() where

import Control.Monad.IO.Class (liftIO)
import Data.UUID
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Sequence.Aggregate