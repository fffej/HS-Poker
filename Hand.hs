{-
  A hand is a collection of 5 cards.

  This file also includes the functions supporting the most naive way of ranking hands
-}
module Hand (
    Hand(..), -- TODO eliminate the destructuring and hence the need for .
    Category(..),
    GroupedRanks,
    biggestGroup,
    biggestValue,
    secondBiggestValue,
    thirdBiggestValue,
    smallestValue,
    isThreeTwoGroup,
    isTwoTwoOneGroup,
    isTwoOneOneOneGroup,
    groupSize,
    mkHand,
    allSameSuit,
    contiguousRanks,
    maxRankInStraight,
    getGroupedRanks
  ) where

import Card (Card,getRank,getSuit,Rank(..))
import Data.Ord (comparing)
import TupleSort (tuple5SortBy)

data Hand = Hand (Card,Card,Card,Card,Card) deriving Show

data Category = CStraightFlush | CFourOfAKind | CFullHouse | CFlush
              | CStraight | CThreeOfAKind | CTwoPairs | COnePair
              | CHighCard deriving (Eq,Ord,Show)

mkHand :: (Card,Card,Card,Card,Card) -> Hand
mkHand x = Hand y
  where
    y = tuple5SortBy (comparing getRank) x
    
data GroupedRanks = FourOneGroup Rank Rank    
                  | ThreeTwoGroup Rank Rank
                  | TwoTwoOneGroup Rank Rank Rank
                  | TwoOneOneOneGroup Rank Rank Rank Rank
                  | OneOneOneOneOneGroup Rank Rank Rank Rank Rank
                    
biggestValue :: GroupedRanks -> Rank                    
biggestValue (FourOneGroup a _) = a
biggestValue (ThreeTwoGroup a _) = a
biggestValue (TwoTwoOneGroup a _ _) = a
biggestValue (TwoOneOneOneGroup a _ _ _) = a
biggestValue (OneOneOneOneOneGroup a _ _ _ _ ) = a

secondBiggestValue :: GroupedRanks -> Rank
secondBiggestValue (FourOneGroup _ a) = a
secondBiggestValue (ThreeTwoGroup _ a) = a
secondBiggestValue (TwoTwoOneGroup _ a _) = a
secondBiggestValue (TwoOneOneOneGroup _ a _ _) = a
secondBiggestValue (OneOneOneOneOneGroup _ a _ _ _ ) = a

thirdBiggestValue :: GroupedRanks -> Rank
thirdBiggestValue (TwoTwoOneGroup _ _ a) = a
thirdBiggestValue (TwoOneOneOneGroup _ _ a _) = a
thirdBiggestValue (OneOneOneOneOneGroup _ _ a _ _) = a
thirdBiggestValue _ = error "There is no third biggest value"

smallestValue :: GroupedRanks -> Rank
smallestValue (FourOneGroup _ a) = a
smallestValue (ThreeTwoGroup _ a) = a
smallestValue (TwoTwoOneGroup _ _ a) = a
smallestValue (TwoOneOneOneGroup _ _ _ a) = a
smallestValue (OneOneOneOneOneGroup _ _ _ _ a) = a


groupSize :: GroupedRanks -> Int
groupSize (FourOneGroup _ _) = 2
groupSize (ThreeTwoGroup _ _) = 2
groupSize (TwoTwoOneGroup _ _ _) = 3
groupSize (TwoOneOneOneGroup _ _ _ _) = 4
groupSize (OneOneOneOneOneGroup _ _ _ _ _) = 5

allEqual3 :: Eq a => a -> a -> a -> Bool
allEqual3 a b c = a == b && b == c

allEqual4 :: Eq a => a -> a -> a -> a -> Bool
allEqual4 a b c d = a == b && b == c && c == d

biggestGroup :: GroupedRanks -> Int
biggestGroup (FourOneGroup _ _) = 4
biggestGroup (ThreeTwoGroup _ _) = 3
biggestGroup (TwoTwoOneGroup _ _ _) = 2
biggestGroup (TwoOneOneOneGroup _ _ _ _) = 2
biggestGroup (OneOneOneOneOneGroup _ _ _ _ _) = 1

isThreeTwoGroup :: GroupedRanks -> Bool
isThreeTwoGroup (ThreeTwoGroup _ _) = True
isThreeTwoGroup _ = False

isTwoTwoOneGroup :: GroupedRanks -> Bool
isTwoTwoOneGroup (TwoTwoOneGroup _ _ _) = True
isTwoTwoOneGroup _ = False

isTwoOneOneOneGroup :: GroupedRanks -> Bool
isTwoOneOneOneGroup (TwoOneOneOneGroup _ _ _ _) = True
isTwoOneOneOneGroup _ = False


-- Take advantage that they are already sorted by rank
getGroupedRanks :: Hand -> GroupedRanks
getGroupedRanks (Hand (a',b',c',d',e')) 
  | allEqual4 a b c d = FourOneGroup a e
  | allEqual4 b c d e = FourOneGroup e a
  | allEqual3 a b c && d == e = ThreeTwoGroup a e
  | allEqual3 c d e && a == b = ThreeTwoGroup c a
  | a == b && c == d = TwoTwoOneGroup a c e
  | a == b && d == e = TwoTwoOneGroup a d c
  | b == c && d == e = TwoTwoOneGroup b d a
  | a == b = TwoOneOneOneGroup a c d e
  | b == c = TwoOneOneOneGroup b a d e
  | c == d = TwoOneOneOneGroup c a b e
  | d == e = TwoOneOneOneGroup d a b c
  | otherwise = OneOneOneOneOneGroup a b c d e 
    where
      a = getRank a'
      b = getRank b'
      c = getRank c'
      d = getRank d'
      e = getRank e'      

{-getGroupedRanks :: Hand -> GroupedRanks
getGroupedRanks (Hand (a,b,c,d,e)) = sortBy (comparing length) $ group values
  where
    values = map getRank [a,b,c,d,e]
-}    

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

     
