module Sequence.Api.EventData where

import Data.Aeson (ToJSON, toJSON, (.=), object)
import Data.UUID

data EventData a = EventData
    { eventId     :: UUID
    , eventData   :: Event a
    , aggregateId :: UUID }    

instance Eq (EventData a) where
    l == r = (eventId l) == (eventId r)

instance Ord (EventData a) where
    compare l r = compare (eventId l) (eventId r)

instance ToJSON a => ToJSON (EventData a) where
	toJSON e =
		object [ "id"          .= eventId e
		       , "event"       .= eventData e
		       , "aggregateId" .= aggregateId e ]