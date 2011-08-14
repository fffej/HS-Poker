module Hand (
    Hand(..), -- TODO eliminate the destructuring and hence the need for ..
    mkHand,
    allSameSuit,
    contiguousValues,
    maxValueInStraight
  ) where

import Card (Card,getValue,getSuit,Value(..))

import Data.List (sortBy,groupBy)
import Data.Ord (comparing)

data Hand = Hand (Card,Card,Card,Card,Card) [[Card]] deriving Show

mkHand :: (Card,Card,Card,Card,Card) -> Hand
mkHand (a,b,c,d,e) = Hand (a',b',c',d',e') gc
  where
    gc = groupedValues [a',b',c',d',e']
    cards = [a,b,c,d,e]
    [a',b',c',d',e'] = sortBy (comparing getValue) cards

groupedValues :: [Card] -> [[Card]]
groupedValues cards = sortBy (comparing length) $ groupBy (\x y -> getValue x == getValue y) cards

allSameSuit :: Hand -> Bool
allSameSuit (Hand (a,b,c,d,e) _) = getSuit a == getSuit b && getSuit b == getSuit c &&
                                   getSuit c == getSuit d && getSuit d == getSuit e

contiguousValues :: Hand -> Bool
contiguousValues (Hand (a,b,c,d,e) gv) 
  | length gv /= 5 = False
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
maxValueInStraight (Hand (a,_,_,_,e) _) 
  | e' == Ace && a' == Two = Five
  | otherwise = e'
    where
      a' = getValue a
      e' = getValue e

     
