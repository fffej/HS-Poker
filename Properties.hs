module Properties where

import HandEvaluator (Evaluator(..))
import CactusKevEvaluator (CactusKev(..), cactusKev)
import SimpleEvaluator (NaiveEvaluator(..), naiveEvaluator)

import Card (Card,mkCard,Suit(..),Rank(..))
import Hand (Hand(..), Category(..), mkHand)

import Test.QuickCheck

instance Arbitrary Suit where
  arbitrary = elements [Spade,Diamond,Club,Heart]
  
instance Arbitrary Rank where
  arbitrary = elements [Two .. Ace]

instance Arbitrary Card where
  arbitrary = do
    suit <- arbitrary
    rank <- arbitrary
    return (mkCard suit rank)

instance Arbitrary Hand where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    return (Hand (a,b,c,d,e))