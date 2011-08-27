module Hand where

import Card
import Data.Bits
import Lookup (lookupFlushes, lookupUnique5, getValueFromProduct)

-- |A hand consists of five cards
data Hand = Hand Card Card Card Card Card

data HandRank = StraightFlush
              | FourOfAKind
              | FullHouse
              | Flush
              | Straight
              | ThreeOfAKind
              | TwoPair
              | OnePair
              | HighCard
                deriving (Show,Eq)

handRankFrom :: Int -> HandRank
handRankFrom x 
  | x > 6185  = HighCard
  | x > 3325  = OnePair
  | x > 2467  = TwoPair
  | x > 1609  = ThreeOfAKind
  | x > 1599  = Straight
  | x > 322   = Flush
  | x > 166   = FullHouse 
  | x > 10    = FourOfAKind
  | otherwise = StraightFlush 

-- |Hands evaluate to a score, higher score = better
evaluate :: Hand -> Int
evaluate (Hand (Card a) (Card b) (Card c) (Card d) (Card e))
  | isFlush = lookupFlushes q 
  | isHighCardHand /= 0 = isHighCardHand
  | otherwise = getValueFromProduct combinedPrimes
    where
      isFlush = 0 /= a .&. b .&. c .&. d .&. e .&. 0xF00
      -- This is the index for flushes and high card hands
      q = fromEnum ((a .|. b .|. c .|. d .|. e) `shiftR` 16 ) -- TODO eliminate fromEnum
      -- This is a very big number, we need to find the index within products and then lookup in values
      combinedPrimes =  fromEnum $ (a .&. 0xFF) * (b .&. 0xFF) * (c .&. 0xFF) * (d .&. 0xFF) * (e .&. 0xFF)
      isHighCardHand = lookupUnique5 q
      

