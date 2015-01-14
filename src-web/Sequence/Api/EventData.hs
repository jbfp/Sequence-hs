{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sequence.Api.EventData where

import Data.Aeson (ToJSON, toJSON, (.=), object)
import Data.Function (on)
import Data.UUID (UUID)
import Sequence.Aggregate (Event)
import Sequence.Game (Game)
import Sequence.Api.Json ()

data EventData a = EventData
    { eventData   :: Event a
    , aggregateId :: UUID
    , version :: Int }

instance Eq (EventData a) where
    l == r = (aggregateId l) == (aggregateId r) && (version l) == (version r)

instance Ord (EventData a) where
    compare = compare `on` version

instance ToJSON (EventData Game) where
	toJSON e =
		object [ "event"       .= eventData e
		       , "aggregateId" .= aggregateId e
		       , "version" .= version e ]