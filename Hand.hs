module Hand (
    Hand(..), -- TODO eliminate the destructuring and hence the need for ..
    GroupedValues,
    mkHand,
    allSameSuit,
    contiguousValues,
    maxValueInStraight,
    getGroupedValues
  ) where

import Card (Card,getValue,getSuit,Value(..))

import Data.List (sortBy,group)
import Data.Ord (comparing)

data Hand = Hand (Card,Card,Card,Card,Card) deriving Show

type GroupedValues = [[Value]]

mkHand :: (Card,Card,Card,Card,Card) -> Hand
mkHand (a,b,c,d,e) = Hand (a',b',c',d',e') 
  where
    [a',b',c',d',e'] = sortBy (comparing getValue) [a,b,c,d,e]

getGroupedValues :: Hand -> GroupedValues
getGroupedValues (Hand (a,b,c,d,e)) = sortBy (comparing length) $ group values
  where
    values = map getValue [a,b,c,d,e]

allSameSuit :: Hand -> Bool
allSameSuit (Hand (a,b,c,d,e)) = getSuit a == getSuit b && getSuit b == getSuit c &&
                                 getSuit c == getSuit d && getSuit d == getSuit e

contiguousValues :: Hand -> Bool
contiguousValues (Hand (a,b,c,d,e)) 
  | not (a' /= b' && b' /= c' && c'/=d' && d'/=e') = False
  | (a' == Two && e' == Ace) && firstFourCardsContiguous = True
  | otherwise = firstFourCardsContiguous && d' /= Ace && succ d' == e'
    where
      a' = getValue a
      b' = getValue b
      c' = getValue c
      d' = getValue d
      e' = getValue e
      firstFourCardsContiguous = succ a' == b' && succ b' == c' && succ c' == d'
  
-- Assumed that it is already a straight!
maxValueInStraight :: Hand -> Value      
maxValueInStraight (Hand (a,_,_,_,e)) 
  | e' == Ace && a' == Two = Five
  | otherwise = e'
    where
      a' = getValue a
      e' = getValue e

     
