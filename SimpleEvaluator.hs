module SimpleEvaluator (
  NaiveEvaluator(..),
  naiveEvaluator
  ) where

import Card (Rank(..),getRank)
import Hand (
  Hand(..),
  Category(..),
  GroupedRanks,
  biggestValue,
  secondBiggestValue,
  thirdBiggestValue,
  isThreeTwoGroup,
  isThreeOneOneGroup,
  isTwoTwoOneGroup,
  isTwoOneOneOneGroup,
  allSameSuit,
  groupSize,
  biggestGroup,
  contiguousRanks,
  maxRankInStraight,
  smallestValue,
  getGroupedRanks,
  sortHand
  )
  
import HandEvaluator(Evaluator(..))
import Data.Ord (comparing)  

-- TODO something about this doesn't seem right!
data NaiveEvaluator = NaiveEvaluator  
  
-- The "red-neck" naive evaluator
naiveEvaluator :: NaiveEvaluator
naiveEvaluator = NaiveEvaluator

instance Evaluator NaiveEvaluator where
  scoreHand _ hand = score $ getBestHand (sortHand hand)
  getCategory _ hand = handCategory $ getBestHand (sortHand hand)
    where
      handCategory :: BestHand -> Category
      handCategory (StraightFlush _) = CStraightFlush
      handCategory (FourOfAKind _ _) = CFourOfAKind
      handCategory (FullHouse _ _) = CFullHouse
      handCategory (Flush _ _ _ _ _) = CFlush
      handCategory (Straight _) = CStraight
      handCategory (ThreeOfAKind _ _ _) = CThreeOfAKind
      handCategory (TwoPairs _ _ _) = CTwoPairs
      handCategory (OnePair _ _ _ _) = COnePair
      handCategory (HighCard _ _ _ _ _) = CHighCard


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
                         
-- Translate a best hand to a human readable category

instance Ord BestHand where
  compare = comparing score

maxHighCard :: Int
maxHighCard = score (HighCard Ace Ace Ace Ace Ace)

maxOnePair :: Int
maxOnePair = score (OnePair Ace Ace Ace Ace)

maxTwoPairs :: Int
maxTwoPairs = score (TwoPairs Ace Ace Ace)

maxThreeOfAKind :: Int
maxThreeOfAKind = score (ThreeOfAKind Ace Ace Ace)

maxStraight :: Int
maxStraight = score (Straight Ace)

maxFlush :: Int
maxFlush = score (Flush Ace Ace Ace Ace Ace)

maxFullHouse :: Int
maxFullHouse = score (FullHouse Ace Ace)

maxFourOfAKind :: Int
maxFourOfAKind = score (FourOfAKind Ace Ace)

maxStraightFlush :: Int
maxStraightFlush = score (StraightFlush Ace)

score :: BestHand -> Int
score (HighCard a b c d e) = fromEnum a * 20736 + fromEnum b * 1728 + fromEnum c * 144 + fromEnum d * 12 + fromEnum e
score (OnePair a b c d) = maxHighCard + (fromEnum a * 1728) + (fromEnum b * 144) + (fromEnum c * 12) + fromEnum d
score (TwoPairs a b c) = maxOnePair + (fromEnum a * 144) + (fromEnum b * 12) + fromEnum c
score (ThreeOfAKind a b c) = maxTwoPairs + (fromEnum a * 144) + (fromEnum b * 12) + fromEnum c
score (Straight a) = maxThreeOfAKind + fromEnum a
score (Flush a b c d e) = maxStraight + fromEnum a * 20736 + fromEnum b * 1728 + fromEnum c * 144 + fromEnum d * 12 + fromEnum e
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
isFourOfAKind groupedRanks = groupSize groupedRanks == 2 && biggestGroup groupedRanks == 4

mkFourOfAKind :: GroupedRanks -> BestHand
mkFourOfAKind groupedRanks = FourOfAKind (biggestValue groupedRanks) (secondBiggestValue groupedRanks)

isFullHouse :: GroupedRanks -> Bool
isFullHouse groupedRanks = groupSize groupedRanks == 2 && biggestGroup groupedRanks == 3

mkFullHouse :: GroupedRanks -> BestHand
mkFullHouse groupedRanks = FullHouse (biggestValue groupedRanks) (secondBiggestValue groupedRanks)
  
isFlush :: Hand -> Bool
isFlush = allSameSuit    

mkFlush :: Hand -> BestHand
mkFlush (Hand (a,b,c,d,e)) = Flush (getRank e) (getRank d) (getRank c) (getRank b) (getRank a)

isStraight :: Hand -> Bool
isStraight = contiguousRanks

mkStraight :: Hand -> BestHand
mkStraight hand = Straight (maxRankInStraight hand)

isThreeOfAKind :: GroupedRanks -> Bool
isThreeOfAKind groupedRanks = groupSize groupedRanks == 3 && isThreeOneOneGroup groupedRanks

mkThreeOfAKind :: GroupedRanks -> BestHand
mkThreeOfAKind groupedRanks = ThreeOfAKind (biggestValue groupedRanks) (secondBiggestValue groupedRanks) (thirdBiggestValue groupedRanks)
                                         
isTwoPairs :: GroupedRanks -> Bool
isTwoPairs groupedRanks = groupSize groupedRanks == 3 && isTwoTwoOneGroup groupedRanks

mkTwoPairs :: GroupedRanks -> BestHand
mkTwoPairs groupedRanks = TwoPairs (biggestValue groupedRanks) (secondBiggestValue groupedRanks) (thirdBiggestValue groupedRanks)
                                     
isOnePair :: GroupedRanks -> Bool
isOnePair groupedRanks = groupSize groupedRanks == 4 && isTwoOneOneOneGroup groupedRanks

mkOnePair :: GroupedRanks -> BestHand
mkOnePair groupedRanks = OnePair (biggestValue groupedRanks) (secondBiggestValue groupedRanks) (thirdBiggestValue groupedRanks) (smallestValue groupedRanks)

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
