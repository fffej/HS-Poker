module Deck where

import Card
import Hand

import Data.Ord (comparing)

import Data.List
import Data.Maybe
import qualified Data.Vector as V

type CardVector = V.Vector Card

data Deck = Deck CardVector deriving Show

getCard :: Deck -> Int -> Card
getCard (Deck xs) n = xs V.! n

data BestHand = StraightFlush Value -- highest card
              | FourOfAKind Value Value -- four of a kind, plus kicker
              | FullHouse Value Value -- 3 and 2
              | Flush Value Value Value Value Value -- highest card
              | Straight Value -- highest card
              | ThreeOfAKind Value Value Value -- three of a kind, plus kickers
              | TwoPairs Value Value Value -- two pairs, plus kicker
              | OnePair Value Value Value Value -- one pair, 3 kickers
              | HighCard Value Value Value Value Value
                deriving (Show,Eq)  

instance Ord BestHand where
  compare = comparing score

maxHighCard = score (HighCard Ace Ace Ace Ace Ace)
maxOnePair = score (OnePair Ace Ace Ace Ace)
maxTwoPairs = score (TwoPairs Ace Ace Ace)
maxThreeOfAKind = score (ThreeOfAKind Ace Ace Ace)
maxStraight = score (Straight Ace)
maxFlush = score (Flush Ace Ace Ace Ace Ace)
maxFullHouse = score (FullHouse Ace Ace)
maxFourOfAKind = score (FourOfAKind Ace Ace)
maxStraightFlush = score (StraightFlush Ace)

score :: BestHand -> Int
score (HighCard a b c d e) = fromEnum a * (12 ^ 4) + fromEnum b * (12 ^ 3) + fromEnum c * (12 ^ 2)  + fromEnum d * 12 + fromEnum e
score (OnePair a b c d) = maxHighCard + (fromEnum a * 12^3) + (fromEnum b * 12^2) + (fromEnum c * 12) + fromEnum d
score (TwoPairs a b c) = maxOnePair + (fromEnum a * 12^2) + (fromEnum b * 12) + fromEnum c
score (ThreeOfAKind a b c) = maxTwoPairs + (fromEnum a * 12^2) + (fromEnum b * 12) + fromEnum c
score (Straight a) = maxThreeOfAKind + (fromEnum a)
score (Flush a b c d e) = maxStraight + fromEnum a * (12 ^ 4) + fromEnum b * (12 ^ 3) + fromEnum c * (12 ^ 2)  + fromEnum d * 12 + fromEnum e
score (FullHouse a b) = maxFlush + fromEnum a * 12 + fromEnum b
score (FourOfAKind a b) = maxFullHouse + fromEnum a * 12 + fromEnum b
score (StraightFlush a) = maxFourOfAKind + fromEnum a

isStraightFlush :: Hand -> Bool
isStraightFlush hand = allSameSuit hand && isStraight hand
    
mkStraightFlush :: Hand -> BestHand
mkStraightFlush hand = StraightFlush v
  where
    (Straight v) = mkStraight hand

isFourOfAKind :: GroupedValues -> Bool
isFourOfAKind groupedValues = length groupedValues == 2 && length (head groupedValues) == 1

mkFourOfAKind :: GroupedValues -> BestHand
mkFourOfAKind groupedValues = FourOfAKind (head $ last groupedValues) (head $ head groupedValues)

isFullHouse :: GroupedValues -> Bool
isFullHouse groupedValues = length groupedValues == 2 && length (head groupedValues) == 2 

mkFullHouse :: GroupedValues -> BestHand
mkFullHouse groupedValues = FullHouse (head $ last groupedValues) (head $ head groupedValues)
  
isFlush :: Hand -> Bool
isFlush = allSameSuit    

mkFlush :: Hand -> BestHand
mkFlush (Hand (a,b,c,d,e)) = Flush (getValue e) (getValue d) (getValue c) (getValue b) (getValue a)

isStraight :: Hand -> Bool
isStraight = contiguousValues

mkStraight :: Hand -> BestHand
mkStraight hand = Straight (maxValueInStraight hand)

isThreeOfAKind :: GroupedValues -> Bool
isThreeOfAKind groupedValues = length groupedValues == 3 && length (last groupedValues) == 3

mkThreeOfAKind :: GroupedValues -> BestHand
mkThreeOfAKind groupedValues = ThreeOfAKind threeValue maxKickerVal minKickerVal
  where
    threeValue = head (last groupedValues)
    (minKickerVal:maxKickerVal:[]) = sort (head (head groupedValues) : head (tail groupedValues))
                                         
isTwoPairs :: GroupedValues -> Bool
isTwoPairs groupedValues = (length groupedValues) == 3  && length (last groupedValues) == 2 

mkTwoPairs :: GroupedValues -> BestHand
mkTwoPairs groupedValues = TwoPairs highPair lowPair kicker
  where
    [kicker,lowPair,highPair] = map head groupedValues                                     
                                     
isOnePair :: GroupedValues -> Bool
isOnePair groupedValues = length groupedValues == 4 && length (last groupedValues) == 2

mkOnePair :: GroupedValues -> BestHand
mkOnePair groupedValues = OnePair maxValue k3 k2 k1
  where
    (k1:k2:k3:[]) = map head (init groupedValues)
    maxValue = head (last groupedValues)

mkHighCard :: Hand -> BestHand
mkHighCard (Hand (a,b,c,d,e)) = HighCard (getValue e) (getValue d) (getValue c) (getValue b) (getValue a)

getBestHand :: Hand -> BestHand
getBestHand hand
  | isStraightFlush hand = mkStraightFlush hand
  | isFourOfAKind groupedValues = mkFourOfAKind groupedValues
  | isFullHouse groupedValues = mkFullHouse groupedValues
  | isFlush hand = mkFlush hand
  | isStraight hand = mkStraight hand
  | isThreeOfAKind groupedValues =  mkThreeOfAKind groupedValues
  | isTwoPairs groupedValues = mkTwoPairs groupedValues
  | isOnePair groupedValues = mkOnePair groupedValues
  | otherwise = mkHighCard hand
    where
      groupedValues = getGroupedValues hand

createOrderedDeck :: Deck
createOrderedDeck = Deck $ V.fromList [Card suit value | suit <- [Hearts,Diamonds,Spades,Clubs], value <- enumFromTo Two Ace]

analyseDeck :: Deck -> [(Int,Int,Int,Int,Int)] -> [(Hand,BestHand)]
analyseDeck d = map (getFiveCardsHand d) 

getFiveCardsHand :: Deck -> (Int,Int,Int,Int,Int) -> (Hand,BestHand)
getFiveCardsHand dk (a,b,c,d,e) = (cards,getBestHand cards)
  where
    cards = mkHand (a',b',c',d',e')
    [a',b',c',d',e']  = map (getCard dk) [a,b,c,d,e]
  
