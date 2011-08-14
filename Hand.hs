module Hand (
    Hand(..), -- TODO eliminate the destructuring and hence the need for ..
    mkHand,
    allSameSuit
  ) where

import Card (Card,getValue,getSuit)

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
