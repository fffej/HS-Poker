module Properties where

import HandEvaluator (Evaluator(..))
import CactusKevEvaluator (CactusKev(..), cactusKevEvaluator)
import SimpleEvaluator (NaiveEvaluator(..), naiveEvaluator)

import Card (Card,mkCard,Suit(..),Rank(..))
import Hand (Hand(..), Category(..), mkHand, getGroupedRanks)

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

-- |Note that mkHand is only needed for the case when 
instance Arbitrary Hand where
  arbitrary = do
    a <- arbitrary
    b <- suchThat arbitrary (/= a)
    c <- suchThat arbitrary (\x -> not $ x `elem` [a,b])
    d <- suchThat arbitrary (\x -> not $ x `elem` [a,b,c])
    e <- suchThat arbitrary (\x -> not $ x `elem` [a,b,c,d])
    return (mkHand (a,b,c,d,e))
    
-- Does the model implementation (naive) match the cactus kev implementation?
prop_modelHandCategory :: Hand -> Bool
prop_modelHandCategory hand = categoryNaive == categoryCactus
  where
    categoryNaive  = getCategory naiveEvaluator hand
    categoryCactus = getCategory cactusKevEvaluator hand
    
prop_modelScoreAgree :: Hand -> Hand -> Bool
prop_modelScoreAgree a b = (compare n1 n2) == (compare c1 c2)
  where
    n1 = scoreHand naiveEvaluator a
    n2 = scoreHand naiveEvaluator b
    c1 = scoreHand cactusKevEvaluator a
    c2 = scoreHand cactusKevEvaluator b