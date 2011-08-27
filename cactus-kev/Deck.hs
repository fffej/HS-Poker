module Deck where

import Card
import Hand
import Data.Word

combinations :: Word32 -> Word32 -> [[Word32]]
combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

combinations k n = combinationsOf k [1..n]

mkHand :: [Word32] -> Hand
mkHand [a,b,c,d,e] = Hand (Card a') (Card b') (Card c') (Card d') (Card e')
  where
    [a',b',c',d',e'] = [a,b,c,d,e]
mkHand _ = error "I can only make a hnad with enough numbers"

main :: IO ()
main = do
  putStrLn "Evaluating hands"
  let possibleHands = combinations 5 52
  let handCombinations = map mkHand possibleHands
      evaluatedHands = map evaluate handCombinations
  putStrLn $ "Number of hands evaluated = " ++ (show $ length evaluatedHands)
  return ()
