import Data.List
import Sequence.Aggregate
import Sequence.Board
import Sequence.Cards
import Sequence.Domain

main :: IO ()
main = do
  print $ execute zero $ Create (GameId 42) capacity
  print $ execute zero $ Join $ Human 42

  print $ execute (replay [created, Joined $ Human 42]) $ Join $ Bot "Alice"
  print $ execute (replay [created, Joined $ Human 42]) $ Start $ Seed 42

  print $ execute (replay [created, Joined $ Human 42, Joined $ Bot "Bob", started]) $ PerformMove (Bot "Bob") 2 2 (Card Diamonds Ace)

  let game = replay [created, Joined $ Human 42, Joined $ Bot "Bob", started]

  case game of
    Playing _ _ board -> print board    

  print $ fmap (\ss -> fmap (snd) ss) $ transpose $ mkBoard

  print $ (replay [created, Joined $ Human 42, Joined $ Bot "Bob", started])
  print $ execute (replay [created, Joined $ Human 42, Joined $ Bot "Bob", started]) $ PerformMove (Bot "Bob") 2 2 (Card Spades Six)
  print $ execute (replay [created, Joined $ Human 42, Joined $ Bot "Bob", started]) $ PerformMove (Human 42) 2 2 (Card Spades Ace)
  where capacity = Capacity { numTeams = 2, numPlayersPerTeam = 1 }
        created = Created (GameId 42) capacity
        started = Started (Seed 42)