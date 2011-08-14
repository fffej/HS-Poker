module DeckTest where

import Deck
import Hand
import Card

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
straightHand = [cc h 2, cc d 3, cc h 5, cc h 6, cc h 4]
straightHandAce = [cc h 14, cc h 2, cc h 4, cc h 5, cc h 3]
threeOfAKindHand = [cc h 14, cc d 14, cc s 14, cc h 9, cc h 5]
twoPairsHand = [cc h 14, cc d 14, cc s 9, cc h 9, cc h 5]
onePairAceHand = [cc h 14, cc d 14, cc c 7, cc d 4, cc d 2]

getBestHandFromList :: [Card] -> BestHand
getBestHandFromList (a:b:c:d:e:[]) = getBestHand (mkHand (a,b,c,d,e))

handFromList :: (Hand -> Maybe BestHand) -> [Card] -> Maybe BestHand
handFromList  f (a:b:c:d:e:[]) = f (mkHand (a,b,c,d,e))

straightFlushFromList = handFromList straightFlush
fourOfAKindFromList = handFromList fourOfAKind
flushFromList = handFromList flush
fullHouseFromList = handFromList fullHouse
straightFromList = handFromList straight
threeOfAKindFromList = handFromList threeOfAKind
twoPairsFromList = handFromList twoPairs
onePairFromList = handFromList onePair

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
                   "Straight hand" (Just $ Straight Six) (straightFromList straightHand)
                   
testStraightHandAce = TestCase $ assertEqual
                      "Straight hand (ace handling)" (Just $ Straight Five) (straightFromList straightHandAce)

testStraightHandNegative = TestCase $ assertEqual
                          "Not a straight" Nothing (straightFromList fourOfAKindHand)
                          
testThreeOfAKindPositive = TestCase $ assertEqual
                           "Three of a kind" (Just (ThreeOfAKind Ace Nine Five)) (threeOfAKindFromList threeOfAKindHand)

testTwoPairsHandPositive = TestCase $ assertEqual
                      "Two Pairs (Aces, Nines, Five)" (Just $ TwoPairs Ace Nine Five) (twoPairsFromList twoPairsHand)

testTwoPairsNegative = TestCase $ assertEqual
                          "Not two pairs" Nothing (twoPairsFromList flushHand)
                          
testThreeOfAKindNegative = TestCase $ assertEqual
                           "Not three of a kind!" Nothing (threeOfAKindFromList straightHandAce)
                           
testOnePairPositive = TestCase $ assertEqual
                      "Single pair" (Just $ OnePair Ace Seven Four Two) (onePairFromList onePairAceHand)
                           
                           

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
  , testThreeOfAKindPositive
  , testTwoPairsHandPositive
  , testTwoPairsNegative
  , testThreeOfAKindNegative
  , testOnePairPositive
  ]
