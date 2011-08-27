module CardDeck (
    Deck,
    createOrderedDeck,
    getCard,
    getCards
  ) where

import Card

import qualified Data.Vector as V

type CardVector = V.Vector Card

data Deck = Deck CardVector deriving Show

getCard :: Deck -> Int -> Card
getCard (Deck xs) n = xs V.! n

getCards :: Deck -> [Card]
getCards (Deck v) = V.toList v


createOrderedDeck :: Deck
createOrderedDeck = Deck $ V.fromList createListOfCards

createListOfCards :: [Card]
createListOfCards = [mkCard r s | r <- [Two .. Ace], s <- [Heart .. Spade]]



