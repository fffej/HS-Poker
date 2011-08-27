{-
  A hand is a collection of 5 cards.

  This file also includes the functions supporting the most naive way of ranking hands
-}
module Hand (
    Hand(..), -- TODO eliminate the destructuring and hence the need for ..
    GroupedRanks,
    mkHand,
    allSameSuit,
    contiguousRanks,
    maxRankInStraight,
    getGroupedRanks
  ) where

import Card (Card,getRank,getSuit,Rank(..))

import Data.List (sortBy,group)
import Data.Ord (comparing)

import TupleSort (tuple5SortBy)

data Hand = Hand (Card,Card,Card,Card,Card) deriving Show

type GroupedRanks = [[Rank]]

mkHand :: (Card,Card,Card,Card,Card) -> Hand
mkHand x = Hand y
  where
    y = tuple5SortBy (comparing getRank) x
--    [a',b',c',d',e'] = sortBy (comparing getRank) [a,b,c,d,e]

getGroupedRanks :: Hand -> GroupedRanks
getGroupedRanks (Hand (a,b,c,d,e)) = sortBy (comparing length) $ group values
  where
    values = map getRank [a,b,c,d,e]
    
    

allSameSuit :: Hand -> Bool
allSameSuit (Hand (a,b,c,d,e)) = getSuit a == getSuit b && getSuit b == getSuit c &&
                                 getSuit c == getSuit d && getSuit d == getSuit e

contiguousRanks :: Hand -> Bool
contiguousRanks (Hand (a,b,c,d,e)) 
  | not (a' /= b' && b' /= c' && c'/=d' && d'/=e') = False
  | (a' == Two && e' == Ace) && firstFourCardsContiguous = True
  | otherwise = firstFourCardsContiguous && d' /= Ace && succ d' == e'
    where
      a' = getRank a
      b' = getRank b
      c' = getRank c
      d' = getRank d
      e' = getRank e
      firstFourCardsContiguous = succ a' == b' && succ b' == c' && succ c' == d'
  
-- Assumed that it is already a straight!
maxRankInStraight :: Hand -> Rank      
maxRankInStraight (Hand (a,_,_,_,e)) 
  | e' == Ace && a' == Two = Five
  | otherwise = e'
    where
      a' = getRank a
      e' = getRank e

     
