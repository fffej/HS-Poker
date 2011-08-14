module DeckAnalysis (main) where

import Card
import Deck
import Hand
import Choose

import Data.Map (Map)
import qualified Data.Map as M

data Category = CStraightFlush | CFourAKind | CFullHouse | CFlush
              | CStraight | CThreeOfAKind | CTwoPairs | COnePair
              | CHighCard deriving (Eq,Ord,Show)

getCategory :: BestHand -> Category
getCategory (StraightFlush _) = CStraightFlush
getCategory (FourOfAKind _ _) = CFourAKind
getCategory (FullHouse _ _) = CFullHouse
getCategory (Flush _ _ _ _ _) = CFlush
getCategory (Straight _) = CStraight
getCategory (ThreeOfAKind _ _ _) = CThreeOfAKind
getCategory (TwoPairs _ _ _) = CTwoPairs
getCategory (OnePair _ _ _ _) = COnePair
getCategory (HighCard _ _ _ _ _) = CHighCard

type HandCounts = Map Category Int

insertCategory :: HandCounts -> (Hand,BestHand) -> HandCounts
insertCategory handCounts (_, bestHand) = M.insertWith' (+) category 1 handCounts
  where
    category = getCategory bestHand
main :: IO ()
main = do
  let d = createOrderedDeck
      hack [a,b,c,d,e] = (a-1,b-1,c-1,d-1,e-1)
      bestHands = analyseDeck d (map hack $ combinations 5 52)
      categories = foldl insertCategory M.empty bestHands
  
  print categories
  
  return ()
  
