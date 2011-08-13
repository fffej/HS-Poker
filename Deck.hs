module Deck where

import Data.List
import Data.Maybe
import Data.Ord

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

data Deck = Deck [Card] deriving Show

data Hand = Hand [Card]

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
  
getBestHand :: Card -> Card -> Card -> Card -> Card -> BestHand
getBestHand a b c d e | isJust straightFlush' = fromJust straightFlush' 
  where
    straightFlush' = straightFlush a b c d e
                                                             
allSameSuit :: [Card] -> Bool
allSameSuit [] = True
allSameSuit (x:xs) = all (\c -> getSuit x == getSuit c) xs
  
contiguousValues :: [Card] -> Bool
contiguousValues xs | length uniqValues < 5 = False
                    | (highest - lowest) == 4 = True
                    | highest == 12 && aceCondition = True
                    | otherwise = False
  where
    uniqValues = nub $ sort $ map getValue xs 
    lowest = fromEnum $ head uniqValues
    highest = fromEnum $ last uniqValues
    aceCondition = lowest == 2 && (fromEnum $ last (init uniqValues)) == 5

maxValue :: [Card] -> Value
maxValue = maximum . (map getValue)

straightFlush :: Card -> Card -> Card -> Card -> Card -> Maybe BestHand
straightFlush a b c d e | allSameSuit cards && contiguousValues cards = Just $ StraightFlush (maxValue cards)
                        | otherwise = Nothing
  where
    cards = [a,b,c,d,e]

-- TODO express that the length of this resultant list is >=2 && <=5 (at least when supplied with five cards!)
groupedValues :: [Card] -> [[Card]]
groupedValues cards = sortBy (comparing length) $ groupBy (\x y -> getValue x == getValue y) $ sortBy (comparing getValue) cards

fourOfAKind :: Card -> Card -> Card -> Card -> Card -> Maybe BestHand
fourOfAKind a b c d e | length groupedCards /= 2 = Nothing 
                      | length (head groupedCards) /= 1 = Nothing
                      | otherwise = Just $ FourOfAKind (getValue (head $ last groupedCards)) (getValue (head $ head groupedCards))
  where
    groupedCards = groupedValues cards
    cards = [a,b,c,d,e]

fullHouse :: Card -> Card -> Card -> Card -> Card -> Maybe BestHand
fullHouse a b c d e | length groupedCards /= 2 = Nothing 
                      | length (head groupedCards) /= 2 = Nothing
                      | otherwise = Just $ FullHouse (getValue (head $ last groupedCards)) (getValue (head $ head groupedCards))
  where
    groupedCards = groupedValues cards
    cards = [a,b,c,d,e]
    
flush :: Card -> Card -> Card -> Card -> Card -> Maybe BestHand
flush a b c d e | allSameSuit cards = Just $ Flush (maxValue cards)
                | otherwise = Nothing
  where
    cards = [a,b,c,d,e]
    
straight :: Card -> Card -> Card -> Card -> Card -> Maybe BestHand
straight _ _ _ _ _= Nothing -- TODO

threeOfAKind :: Card -> Card -> Card -> Card -> Card -> Maybe BestHand
threeOfAKind a b c d e  | length groupedCards /= 3 = Nothing  
                        | length (last groupedCards) /= 3 = Nothing
                        | otherwise = Just $ ThreeOfAKind threeValue maxKickerVal minKickerVal
  where
    cards = [a,b,c,d,e]
    threeValue = getValue $ head (last groupedCards)
    (minKickerVal:maxKickerVal:[]) = sort (map getValue ((head $ head groupedCards) : (head $ tail groupedCards)))
    groupedCards = groupedValues cards

twoPair :: Card -> Card -> Card -> Card -> Card -> Maybe BestHand
twoPair a b c d e  = Nothing  -- TODO
  where
    cards = [a,b,c,d,e]
    groupedCards = groupedValues cards

onePair :: Card -> Card -> Card -> Card -> Card -> Maybe BestHand
onePair a b c d e = Nothing 
  where
    cards = [a,b,c,d,e]
    groupedCards = groupedValues cards

highCard :: Card -> Card -> Card -> Card -> Card -> BestHand
highCard a b c d e  = HighCard av bv cv dv ev
  where
    cards = sortBy (comparing getValue) [a,b,c,d,e]
    (av:bv:cv:dv:ev:[]) = map getValue cards


createOrderedDeck :: Deck
createOrderedDeck = Deck [Card suit value | suit <- [Hearts,Diamonds,Spades,Clubs], value <- enumFromTo Two Ace]
