module DeckTest where

import Deck
import Test.HUnit

h = 'H'
d = 'D'
s = 'S'
c = 'C'

getBestHandFromList :: [Card] -> BestHand
getBestHandFromList (a:b:c:d:e:[]) = getBestHand a b c d e 

straightFlushFromList :: [Card] -> Maybe BestHand
straightFlushFromList (a:b:c:d:e:[]) = straightFlush a b c d e 

testStraightFlushPositive = TestCase $ assertEqual
               "StraightFlush with value" (Just $ StraightFlush Six) (straightFlushFromList [cc h s| s <- [2,3,4,5,6]]) 

testStraightFlushNegative = TestCase $ assertEqual
               "Not a StraightFlush with value" Nothing (straightFlushFromList [cc h s| s <- [2,3,4,7,6]]) 
               
               


main = runTestTT $ TestList [testStraightFlushPositive,testStraightFlushNegative]
