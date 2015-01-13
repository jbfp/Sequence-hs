module Sequence.Api.EventData where

data EventData a = EventData
    { eventId   :: Int
    , eventData :: Event a }    

instance Eq (EventData a) where
    l == r = (eventId l) == (eventId r)

instance Ord (EventData a) where
    compare l r = compare (eventId l) (eventId r)