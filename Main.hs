module Main (main) where

import CardDeck (Deck,createOrderedDeck,getCard)
import Card (Card)
import Hand (Category(..),Hand(..),mkHand)
import Choose (combinations)

import SimpleEvaluator (NaiveEvaluator(..), naiveEvaluator)
import CactusKevEvaluator (CactusKev(..), cactusKevEvaluator)
import HandEvaluator (Evaluator(..))

import Data.Map (Map)
import qualified Data.Map as M
import Data.List (foldl')

type HandCounts = Map Category Int

insertCategory :: HandCounts -> Category -> HandCounts
insertCategory handCounts category = M.insertWith' (+) category 1 handCounts

tupleUp :: Deck -> [Int] -> (Card,Card,Card,Card,Card)
tupleUp dk [a,b,c,d,e] = (getCard dk a,getCard dk b,getCard dk c,getCard dk d,getCard dk e)
tupleUp _ _ = error "Only works with lists of size 5"

getCategories :: (Evaluator a) => a -> [Hand] -> HandCounts
getCategories ev hands = foldl' insertCategory M.empty (map (getCategory ev) hands)

main :: IO ()
main = do
  let d = createOrderedDeck
      cards = map (mkHand . tupleUp d) $ combinations (5 :: Int) 52
      naiveCategories = getCategories naiveEvaluator cards
      cactusKevCategories = getCategories cactusKevEvaluator cards
  
  print naiveCategories
  print cactusKevCategories
  
  return ()
  
