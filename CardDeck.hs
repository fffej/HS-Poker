module CardDeck (
    Deck,
    createOrderedDeck,
    getCard,
    getCards
  ) where

import Card
import Hand

import Control.Monad.Random
import RandomList

import qualified Data.Vector as V

type CardVector = V.Vector Card

data Deck = Deck CardVector deriving Show

getCard :: Deck -> Int -> Card
getCard (Deck xs) n = xs V.! n

getCards :: Deck -> [Card]
getCards (Deck v) = V.toList v


createOrderedDeck :: Deck
createOrderedDeck = Deck $ V.fromList $ createListOfCards

createListOfCards :: [Card]
createListOfCards = [mkCard r s | r <- [Two .. Ace], s <- [Heart .. Spade]]

getPermutation :: Int -> IO [Int]
getPermutation n = do
        let l = permute [1..n]
        o <- evalRandIO l
        return o

getShuffledDeck :: [Int] -> Deck
getShuffledDeck l = Deck $ V.fromList $ getShuffledDeck' l []
            where
              getShuffledDeck' :: [Int] -> [Card] -> [Card]
              getShuffledDeck' [] c = c
              getShuffledDeck' (n:ns) c = getShuffledDeck' ns ((cards V.! (n - 1)):c)
              cards = V.fromList createListOfCards

