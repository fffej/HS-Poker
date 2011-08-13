module DeckTest where

import Deck
import Test.HUnit

h = 'H'
d = 'D'
s = 'S'
c = 'C'

flushHand = [cc h s | s <- [2,3,4,6,7]]
straightFlushHand = [cc h s | s <- [2,3,4,5,6]]
fourOfAKindHand = [cc h 7, cc s 7, cc s 14, cc c 7, cc d 7]

getBestHandFromList :: [Card] -> BestHand
getBestHandFromList (a:b:c:d:e:[]) = getBestHand a b c d e 

straightFlushFromList :: [Card] -> Maybe BestHand
straightFlushFromList (a:b:c:d:e:[]) = straightFlush a b c d e 

fourOfAKindFromList :: [Card] -> Maybe BestHand
fourOfAKindFromList (a:b:c:d:e:[]) = fourOfAKind a b c d e 

flushFromList :: [Card] -> Maybe BestHand
flushFromList (a:b:c:d:e:[]) = flush a b c d e 

testStraightFlushPositive = TestCase $ assertEqual
               "StraightFlush with value" (Just $ StraightFlush Six) (straightFlushFromList straightFlushHand) 

testStraightFlushNegative = TestCase $ assertEqual
               "Not a StraightFlush with value" Nothing (straightFlushFromList flushHand) 
               
testFourOfAKindPositive = TestCase $ assertEqual
               "Four of a kind with values" (Just $ FourOfAKind Seven Ace) (fourOfAKindFromList fourOfAKindHand) 

testFourOfAKindNegative = TestCase $ assertEqual
               "Not four of a kind with values" Nothing (fourOfAKindFromList flushHand) 
               
testFlushPositive = TestCase $ assertEqual
               "Four of a kind with values" (Just $ FourOfAKind Seven) (flushFromList flushHand) 

testFlushNegative = TestCase $ assertEqual
               "Not four of a kind with values" Nothing (flushFromList fourOfAKindHand) 
               
               


main = runTestTT $ TestList [testStraightFlushPositive,testStraightFlushNegative,testFourOfAKindPositive,testFourOfAKindNegative,testFlushPositive,testFlushNegative]
