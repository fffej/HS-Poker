module Deck where

-- TODO don't export the type constructor Hand, just mkHand

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Vector as V

import Choose

type CardVector = V.Vector Card

data Suit = Hearts | Diamonds | Spades | Clubs deriving (Show,Eq)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
           deriving (Show,Enum,Eq,Ord)

cc :: Char -> Int -> Card
cc c 1 = Card (mkSuit c) Ace
cc c n = Card (mkSuit c) (mkValue n)

mkSuit :: Char -> Suit
mkSuit 'D' = Diamonds
mkSuit 'C' = Clubs
mkSuit 'S' = Spades
mkSuit 'H' = Hearts
mkSuit _ = error "Are you mad?  What type of card is that!"

mkValue :: Int -> Value
mkValue 1 = Ace
mkValue n = toEnum (n - 2)

data Card = Card Suit Value

getSuit :: Card -> Suit
getSuit (Card x _) = x

getValue :: Card -> Value
getValue (Card _ v) = v

instance Show Card where
  show (Card suit value) = show value ++ " of " ++ show suit

data Deck = Deck CardVector deriving Show

getCard :: Deck -> Int -> Card
getCard (Deck xs) n = xs V.! n

data Hand = Hand (Card,Card,Card,Card,Card) deriving Show

mkHand :: (Card,Card,Card,Card,Card) -> Hand
mkHand (a,b,c,d,e) = Hand (a',b',c',d',e')
  where
    cards = [a,b,c,d,e]
    [a',b',c',d',e'] = sortBy (comparing getValue) cards

data BestHand = StraightFlush Value -- highest card
              | FourOfAKind Value Value -- four of a kind, plus kicker
              | FullHouse Value Value -- 3 and 2
              | Flush Value -- highest card
              | Straight Value -- highest card
              | ThreeOfAKind Value Value Value -- three of a kind, plus kickers
              | TwoPairs Value Value Value -- two pairs, plus kicker
              | OnePair Value Value Value Value -- one pair, 3 kickers
              | HighCard Value Value Value Value Value
                deriving (Show,Eq)
  
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

allSameSuit :: Hand -> Bool
allSameSuit (Hand (a,b,c,d,e)) = getSuit a == getSuit b && getSuit b == getSuit c &&
                                 getSuit c == getSuit d && getSuit d == getSuit e
  
contiguousValues :: [Card] -> Bool
contiguousValues xs | length uniqValues < 5 = False
                    | (uniqValues == (enumFromTo lowest highest)) = True
                    | highest == Ace  && aceCondition = True
                    | otherwise = False
  where
    uniqValues = nub $ sort $ map getValue xs 
    lowest = head uniqValues
    highest = last uniqValues
    aceCondition = lowest == Two && (last (init uniqValues)) == Five

maxValue :: [Card] -> Value
maxValue = maximum . (map getValue)

maxValueInStraight :: [Card] -> Value
maxValueInStraight cards | isStraightWithLowAce cards = last $ init sortedValues
                         | otherwise                  = last sortedValues
    where sortedValues = sort $ map getValue cards
          isStraightWithLowAce cards = contiguousValues cards && Ace `elem` sortedValues && Two `elem` sortedValues

straightFlush :: Hand -> Maybe BestHand
straightFlush hand | allSameSuit hand && isJust isStraight = Just $ StraightFlush v
                   | otherwise = Nothing
  where
    isStraight = straight hand
    (Straight v) = fromJust isStraight
    

-- TODO express that the length of this resultant list is >=2 && <=5 (at least when supplied with five cards!)
groupedValues :: [Card] -> [[Card]]
groupedValues cards = sortBy (comparing length) $ groupBy (\x y -> getValue x == getValue y) cards

fourOfAKind :: Hand -> Maybe BestHand
fourOfAKind (Hand (a,b,c,d,e)) | length groupedCards /= 2 = Nothing 
                               | length (head groupedCards) /= 1 = Nothing
                               | otherwise = Just $ FourOfAKind (getValue (head $ last groupedCards)) (getValue (head $ head groupedCards))
  where
    groupedCards = groupedValues cards
    cards = [a,b,c,d,e]

fullHouse :: Hand -> Maybe BestHand
fullHouse (Hand (a,b,c,d,e)) | length groupedCards /= 2 = Nothing 
                             | length (head groupedCards) /= 2 = Nothing
                      | otherwise = Just $ FullHouse (getValue (head $ last groupedCards)) (getValue (head $ head groupedCards))
  where
    groupedCards = groupedValues cards
    cards = [a,b,c,d,e]
    
flush :: Hand -> Maybe BestHand
flush h@(Hand (a,b,c,d,e)) | allSameSuit h = Just $ Flush (getValue e)
                           | otherwise = Nothing
    
straight :: Hand -> Maybe BestHand
straight (Hand (a,b,c,d,e)) | contiguousValues cards = Just $ Straight (maxValueInStraight cards)
                            | otherwise = Nothing
  where
    cards = [a,b,c,d,e]


threeOfAKind :: Hand -> Maybe BestHand
threeOfAKind (Hand (a,b,c,d,e)) | length groupedCards /= 3 = Nothing  
                                | length (last groupedCards) /= 3 = Nothing
                                | otherwise = Just $ ThreeOfAKind threeValue maxKickerVal minKickerVal
  where
    cards = [a,b,c,d,e]
    threeValue = getValue $ head (last groupedCards)
    (minKickerVal:maxKickerVal:[]) = sort (map getValue ((head $ head groupedCards) : (head $ tail groupedCards)))
    groupedCards = groupedValues cards

twoPairs :: Hand -> Maybe BestHand
twoPairs (Hand (a,b,c,d,e)) | length groupedCards /= 3 = Nothing  
                            | length (last groupedCards) /= 2 = Nothing
                            | otherwise = Just $ TwoPairs highPair lowPair kicker
  where
    cards = [a,b,c,d,e]
    [kicker,lowPair,highPair] = map (getValue . head) groupedCards
    groupedCards = groupedValues cards

onePair :: Hand -> Maybe BestHand
onePair (Hand (a,b,c,d,e)) | length groupedCards /= 4 = Nothing 
                           | length (last groupedCards) /= 2 = Nothing
                           | otherwise = Just $ OnePair maxValue k3 k2 k1
  where
    cards = [a,b,c,d,e]
    groupedCards = groupedValues cards
    (k1:k2:k3:[]) = sort (map getValue (map head $ init groupedCards))
    maxValue = getValue $ head (last groupedCards)

highCard :: Hand -> BestHand
highCard (Hand (a,b,c,d,e))  = HighCard (getValue a) (getValue b) (getValue c) (getValue d) (getValue e)

createOrderedDeck :: Deck
createOrderedDeck = Deck $ V.fromList [Card suit value | suit <- [Hearts,Diamonds,Spades,Clubs], value <- enumFromTo Two Ace]

analyseDeck :: Deck -> [(Int,Int,Int,Int,Int)] -> [(Hand,BestHand)]
analyseDeck d choices = map (getFiveCardsHand d) choices

getFiveCardsHand :: Deck -> (Int,Int,Int,Int,Int) -> (Hand,BestHand)
getFiveCardsHand dk (a,b,c,d,e) = (cards,getBestHand cards)
  where
    cards = mkHand (a',b',c',d',e')
    [a',b',c',d',e']  = map (getCard dk) [a,b,c,d,e]
  
