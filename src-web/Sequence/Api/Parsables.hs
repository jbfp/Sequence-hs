{-# LANGUAGE OverloadedStrings #-}

module Sequence.Api.Parsables where

import Data.Aeson
import Data.Text.Lazy
import Data.UUID
import Web.Scotty

instance Parsable UUID where
    parseParam t = maybeToEither "Could not parse UUID." $ fromString $ unpack t
        where maybeToEither _ (Just x) = Right x
              maybeToEither e (Nothing) = Left e