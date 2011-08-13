module DeckTest where

import Deck
import Test.HUnit

h = 'H'
d = 'D'
s = 'S'
c = 'C'

flushHand = [cc h s | s <- [2,3,4,6,7]]
straightFlushHand = [cc h s | s <- [2,3,4,5,6]]
straightFlushHandAce = [cc h s | s <- [14,2,3,4,5]]
fourOfAKindHand = [cc h 7, cc s 7, cc s 14, cc c 7, cc d 7]
fullHouseHand = [cc h 7, cc d 7, cc s 7, cc c 5, cc d 5]
straightHand = [cc h 2, cc h 3, cc h 5, cc h 7, cc h 9]
straightHandAce = [cc h 14, cc h 2, cc h 4, cc h 5, cc h 3]

getBestHandFromList :: [Card] -> BestHand
getBestHandFromList (a:b:c:d:e:[]) = getBestHand a b c d e 

straightFlushFromList :: [Card] -> Maybe BestHand
straightFlushFromList (a:b:c:d:e:[]) = straightFlush a b c d e 

fourOfAKindFromList :: [Card] -> Maybe BestHand
fourOfAKindFromList (a:b:c:d:e:[]) = fourOfAKind a b c d e 

flushFromList :: [Card] -> Maybe BestHand
flushFromList (a:b:c:d:e:[]) = flush a b c d e 

fullHouseFromList :: [Card] -> Maybe BestHand
fullHouseFromList (a:b:c:d:e:[]) = fullHouse a b c d e 

straightFromList :: [Card] -> Maybe BestHand
straightFromList (a:b:c:d:e:[]) = straight a b c d e 

testStraightFlushPositive = TestCase $ assertEqual
               "StraightFlush with value" (Just $ StraightFlush Six) (straightFlushFromList straightFlushHand) 
               
testStraightFlushPositiveAce = TestCase $ assertEqual
               "Straight Flush with Ace" (Just $ StraightFlush Five) (straightFlushFromList straightFlushHandAce)

testStraightFlushNegative = TestCase $ assertEqual
               "Not a StraightFlush with value" Nothing (straightFlushFromList flushHand) 
               
testFourOfAKindPositive = TestCase $ assertEqual
               "Four of a kind with values" (Just $ FourOfAKind Seven Ace) (fourOfAKindFromList fourOfAKindHand) 

testFourOfAKindNegative = TestCase $ assertEqual
               "Not four of a kind with values" Nothing (fourOfAKindFromList flushHand) 
               
testFlushPositive = TestCase $ assertEqual
               "Four of a kind with values" (Just $ Flush Seven) (flushFromList flushHand) 

testFlushNegative = TestCase $ assertEqual
               "Not four of a kind with values" Nothing (flushFromList fourOfAKindHand) 
               
testFullHousePositive = TestCase $ assertEqual               
                        "Full house" (Just $ FullHouse Seven Five) (fullHouseFromList fullHouseHand)
                        
testFullHouseNegative = TestCase $ assertEqual
                        "Not full house" Nothing (fullHouseFromList flushHand)
               
testStraightHand = TestCase $ assertEqual
                   "Straight hand" (Just $ Straight Nine) (straightFromList straightHand)
                   
testStraightHandAce = TestCase $ assertEqual
                      "Straight hand (ace handling)" (Just $ Straight Five) (straightFromList straightHandAce)

testStraightHandNegative = TestCase $ assertEqual
                          "Not a straight" Nothing (straightFromList fourOfAKindHand)

main = runTestTT $ TestList [
    testStraightFlushPositive
  , testStraightFlushPositiveAce
  , testStraightFlushNegative
  , testFourOfAKindPositive
  , testFourOfAKindNegative
  , testFullHousePositive
  , testFullHouseNegative
  , testFlushPositive
  , testFlushNegative
  , testStraightHand
  , testStraightHandAce
  , testStraightHandNegative
  ]
