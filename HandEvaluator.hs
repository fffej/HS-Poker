module HandEvaluator (
    Evaluator(..),
  ) where

import Hand (Category,Hand)

class Evaluator a where
  scoreHand :: a -> Hand -> Int
  getCategory :: a -> Hand -> Category