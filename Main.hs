module Main (main) where

import Card
import CardDeck (Deck,createOrderedDeck,getCard)
import Hand
import Choose
import SimpleEvaluator

import Data.Ord (comparing)
import Data.List (maximumBy)

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

createCards :: [Card] -> (Int,Int,Int,Int,Int) -> Hand
createCards x (a,b,c,d,e)= mkHand (x !! a, x !! b, x !! c, x !! d, x !! e)
  
getBestHandFromCards :: [Card] -> Maybe Hand
getBestHandFromCards cards
  | cardCount < 5 = Nothing
  | otherwise = Just $ maximumBy (comparing (score . getBestHand)) (map (createCards cards . zeroBase) (combinations 5 cardCount))
    where
      cardCount = length cards

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

listToFiveTuple :: [a] -> (a,a,a,a,a)
listToFiveTuple (a:b:c:d:e:xs) = (a,b,c,d,e)

main :: IO ()
main = do
  let d = createOrderedDeck
      bestHands = analyseDeck d (map zeroBase $ combinations 5 52)
      categories = foldl insertCategory M.empty bestHands
  
  print categories
  
  return ()
  
