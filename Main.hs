module Main (main) where

import CardDeck (Deck,createOrderedDeck,getCard)
import Hand
import Choose
import SimpleEvaluator

import Data.Map (Map)
import qualified Data.Map as M

type HandCounts = Map Category Int

data Category = CStraightFlush | CFourAKind | CFullHouse | CFlush
              | CStraight | CThreeOfAKind | CTwoPairs | COnePair
              | CHighCard deriving (Eq,Ord,Show)

getCategory :: BestHand -> Category
getCategory (StraightFlush _) = CStraightFlush
getCategory (FourOfAKind _ _) = CFourAKind
getCategory (FullHouse _ _) = CFullHouse
getCategory (Flush _ _ _ _ _) = CFlush
getCategory (Straight _) = CStraight
getCategory (ThreeOfAKind _ _ _) = CThreeOfAKind
getCategory (TwoPairs _ _ _) = CTwoPairs
getCategory (OnePair _ _ _ _) = COnePair
getCategory (HighCard _ _ _ _ _) = CHighCard

insertCategory :: HandCounts -> (Hand,BestHand) -> HandCounts
insertCategory handCounts (_, bestHand) = M.insertWith' (+) category 1 handCounts
  where
    category = getCategory bestHand

zeroBase :: [Int] -> (Int,Int,Int,Int,Int)
zeroBase [a,b,c,d,e] = (a-1,b-1,c-1,d-1,e-1)
zeroBase _ = error "Only works with lists of size 5"

analyseDeck :: Deck -> [(Int,Int,Int,Int,Int)] -> [(Hand,BestHand)]
analyseDeck d = map (getFiveCardsHand d) 

getFiveCardsHand :: Deck -> (Int,Int,Int,Int,Int) -> (Hand,BestHand)
getFiveCardsHand dk (a,b,c,d,e) = (cards,getBestHand cards)
  where
    cards = mkHand (a',b',c',d',e')
    [a',b',c',d',e']  = map (getCard dk) [a,b,c,d,e]

main :: IO ()
main = do
  let d = createOrderedDeck
      bestHands = analyseDeck d (map zeroBase $ combinations (5 :: Int) 52)
      categories = foldl insertCategory M.empty bestHands
  
  print categories
  
  return ()
  
