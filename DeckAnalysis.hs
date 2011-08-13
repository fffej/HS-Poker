module DeckAnalysis (main) where

import Deck
import Choose

main :: IO ()
main = do
  let d = createOrderedDeck
      hack [a,b,c,d,e] = (a-1,b-1,c-1,d-1,e-1)
      bestHands = analyseDeck d (map hack $ combinations 5 52)
      n = (filter (\(_,x) ->  isStraightFlush x Ace) bestHands)
  putStrLn $ "Found " ++ (show n)
  return ()
  
