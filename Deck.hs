module Deck where

import Card
import Hand

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

isStraightFlush :: BestHand -> Value -> Bool                         
isStraightFlush (StraightFlush v) x | v == x = True
                                    | otherwise = False
isStraightFlush _ _ = False
                         
getBestHand :: Hand -> BestHand
getBestHand h = result
  where
    fs = [straightFlush, fourOfAKind, fullHouse, flush, straight, threeOfAKind, twoPairs, onePair]
    results :: [BestHand]
    results = mapMaybe (\x -> x h) fs
    result = head (results ++ [highCard h])
  
straightFlush :: Hand -> Maybe BestHand
straightFlush hand | allSameSuit hand && isJust isStraight = Just $ StraightFlush v
                   | otherwise = Nothing
  where
    isStraight = straight hand
    (Straight v) = fromJust isStraight
    
fourOfAKind :: Hand -> Maybe BestHand
fourOfAKind (Hand _ groupedCards ) | length groupedCards /= 2 = Nothing 
                                   | length (head groupedCards) /= 1 = Nothing
                                   | otherwise = Just $ FourOfAKind (getValue (head $ last groupedCards)) (getValue (head $ head groupedCards))

fullHouse :: Hand -> Maybe BestHand
fullHouse (Hand _ groupedCards) | length groupedCards /= 2 = Nothing 
                                | length (head groupedCards) /= 2 = Nothing
                                | otherwise = Just $ FullHouse (getValue (head $ last groupedCards)) (getValue (head $ head groupedCards))

    
flush :: Hand -> Maybe BestHand
flush h@(Hand (a,b,c,d,e) _ ) | allSameSuit h = Just $ Flush (getValue e) (getValue d) (getValue c) (getValue b) (getValue a)
                              | otherwise = Nothing
    
straight :: Hand -> Maybe BestHand
straight hand | contiguousValues hand = Just $ Straight (maxValueInStraight hand)
              | otherwise = Nothing

threeOfAKind :: Hand -> Maybe BestHand
threeOfAKind (Hand (a,b,c,d,e) groupedCards) | length groupedCards /= 3 = Nothing  
                                             | length (last groupedCards) /= 3 = Nothing
                                             | otherwise = Just $ ThreeOfAKind threeValue maxKickerVal minKickerVal
  where
    threeValue = getValue $ head (last groupedCards)
    (minKickerVal:maxKickerVal:[]) = sort (map getValue (head (head groupedCards) : head (tail groupedCards)))

twoPairs :: Hand -> Maybe BestHand
twoPairs (Hand _ groupedCards) | length groupedCards /= 3 = Nothing  
                               | length (last groupedCards) /= 2 = Nothing
                               | otherwise = Just $ TwoPairs highPair lowPair kicker
  where
    [kicker,lowPair,highPair] = map (getValue . head) groupedCards

onePair :: Hand -> Maybe BestHand
onePair (Hand _ groupedCards) | length groupedCards /= 4 = Nothing 
                              | length (last groupedCards) /= 2 = Nothing
                              | otherwise = Just $ OnePair maxValue k3 k2 k1
  where
    (k1:k2:k3:[]) = map (getValue . head) (init groupedCards)
    maxValue = getValue $ head (last groupedCards)

highCard :: Hand -> BestHand
highCard (Hand (a,b,c,d,e) _)  = HighCard (getValue e) (getValue d) (getValue c) (getValue b) (getValue a)

createOrderedDeck :: Deck
createOrderedDeck = Deck $ V.fromList [Card suit value | suit <- [Hearts,Diamonds,Spades,Clubs], value <- enumFromTo Two Ace]

analyseDeck :: Deck -> [(Int,Int,Int,Int,Int)] -> [(Hand,BestHand)]
analyseDeck d = map (getFiveCardsHand d) 

getFiveCardsHand :: Deck -> (Int,Int,Int,Int,Int) -> (Hand,BestHand)
getFiveCardsHand dk (a,b,c,d,e) = (cards,getBestHand cards)
  where
    cards = mkHand (a',b',c',d',e')
    [a',b',c',d',e']  = map (getCard dk) [a,b,c,d,e]
  
