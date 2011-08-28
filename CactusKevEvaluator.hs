module CactusKevEvaluator where

-- Implementation details taken from http://www.suffecool.net/poker/evaluator.html

import HandEvaluator (Evaluator(..))
import Hand (Hand(..), Category(..))
import Card (Card(..))
import Data.Bits ((.&.),(.|.),shiftR)
import LookupPatternMatch (lookupFlushes, lookupUnique5, getValueFromProduct)

-- Should perhaps keep the lookup tables in here?  Again, something not right
data CactusKev = CactusKev

cactusKevEvaluator :: CactusKev
cactusKevEvaluator = CactusKev

maxScore :: Int
maxScore = 10000

instance Evaluator CactusKev where
  scoreHand _ hand = maxScore - evaluate hand
  getCategory _ hand = (handRankFrom . evaluate) hand

-- |A hand consists of five cards
handRankFrom :: Int -> Category
handRankFrom x 
  | x > 6185  = CHighCard
  | x > 3325  = COnePair
  | x > 2467  = CTwoPairs
  | x > 1609  = CThreeOfAKind
  | x > 1599  = CStraight
  | x > 322   = CFlush
  | x > 166   = CFullHouse 
  | x > 10    = CFourOfAKind
  | otherwise = CStraightFlush 

-- |Hands evaluate to a score, higher score = better
evaluate :: Hand -> Int
evaluate (Hand ((Card a),(Card b),(Card c),(Card d),(Card e)))
  | isFlush =  lookupFlushes q 
  | isHighCardHand /= 0 = isHighCardHand
  | otherwise = getValueFromProduct combinedPrimes
    where
      isFlush = 0 /= a .&. b .&. c .&. d .&. e .&. 0xF000
      -- This is the index for flushes and high card hands
      q = fromEnum ((a .|. b .|. c .|. d .|. e) `shiftR` 16 ) -- TODO eliminate fromEnum
      -- This is a very big number, we need to find the index within products and then lookup in values
      combinedPrimes =  fromEnum $ (a .&. 0xFF) * (b .&. 0xFF) * (c .&. 0xFF) * (d .&. 0xFF) * (e .&. 0xFF)
      isHighCardHand = lookupUnique5 q
      

