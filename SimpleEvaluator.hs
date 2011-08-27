module SimpleEvaluator where

import Card
import Hand
import CardDeck (Deck)

import Data.Ord (comparing)

import Data.List
import Data.Maybe

data BestHand = StraightFlush Rank -- highest card
              | FourOfAKind Rank Rank -- four of a kind, plus kicker
              | FullHouse Rank Rank -- 3 and 2
              | Flush Rank Rank Rank Rank Rank -- highest card
              | Straight Rank -- highest card
              | ThreeOfAKind Rank Rank Rank -- three of a kind, plus kickers
              | TwoPairs Rank Rank Rank -- two pairs, plus kicker
              | OnePair Rank Rank Rank Rank -- one pair, 3 kickers
              | HighCard Rank Rank Rank Rank Rank
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

isFourOfAKind :: GroupedRanks -> Bool
isFourOfAKind groupedRanks = length groupedRanks == 2 && length (head groupedRanks) == 1

mkFourOfAKind :: GroupedRanks -> BestHand
mkFourOfAKind groupedRanks = FourOfAKind (head $ last groupedRanks) (head $ head groupedRanks)

isFullHouse :: GroupedRanks -> Bool
isFullHouse groupedRanks = length groupedRanks == 2 && length (head groupedRanks) == 2 

mkFullHouse :: GroupedRanks -> BestHand
mkFullHouse groupedRanks = FullHouse (head $ last groupedRanks) (head $ head groupedRanks)
  
isFlush :: Hand -> Bool
isFlush = allSameSuit    

mkFlush :: Hand -> BestHand
mkFlush (Hand (a,b,c,d,e)) = Flush (getRank e) (getRank d) (getRank c) (getRank b) (getRank a)

isStraight :: Hand -> Bool
isStraight = contiguousRanks

mkStraight :: Hand -> BestHand
mkStraight hand = Straight (maxRankInStraight hand)

isThreeOfAKind :: GroupedRanks -> Bool
isThreeOfAKind groupedRanks = length groupedRanks == 3 && length (last groupedRanks) == 3

mkThreeOfAKind :: GroupedRanks -> BestHand
mkThreeOfAKind groupedRanks = ThreeOfAKind threeRank maxKickerVal minKickerVal
  where
    threeRank = head (last groupedRanks)
    (minKickerVal:maxKickerVal:[]) = sort (head (head groupedRanks) : head (tail groupedRanks))
                                         
isTwoPairs :: GroupedRanks -> Bool
isTwoPairs groupedRanks = (length groupedRanks) == 3  && length (last groupedRanks) == 2 

mkTwoPairs :: GroupedRanks -> BestHand
mkTwoPairs groupedRanks = TwoPairs highPair lowPair kicker
  where
    [kicker,lowPair,highPair] = map head groupedRanks                                     
                                     
isOnePair :: GroupedRanks -> Bool
isOnePair groupedRanks = length groupedRanks == 4 && length (last groupedRanks) == 2

mkOnePair :: GroupedRanks -> BestHand
mkOnePair groupedRanks = OnePair maxRank k3 k2 k1
  where
    (k1:k2:k3:[]) = map head (init groupedRanks)
    maxRank = head (last groupedRanks)

mkHighCard :: Hand -> BestHand
mkHighCard (Hand (a,b,c,d,e)) = HighCard (getRank e) (getRank d) (getRank c) (getRank b) (getRank a)

getBestHand :: Hand -> BestHand
getBestHand hand
  | isStraightFlush hand = mkStraightFlush hand
  | isFourOfAKind groupedRanks = mkFourOfAKind groupedRanks
  | isFullHouse groupedRanks = mkFullHouse groupedRanks
  | isFlush hand = mkFlush hand
  | isStraight hand = mkStraight hand
  | isThreeOfAKind groupedRanks =  mkThreeOfAKind groupedRanks
  | isTwoPairs groupedRanks = mkTwoPairs groupedRanks
  | isOnePair groupedRanks = mkOnePair groupedRanks
  | otherwise = mkHighCard hand
    where
      groupedRanks = getGroupedRanks hand
