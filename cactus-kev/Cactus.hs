module Cactus where

import Data.Word
import Data.Bits

data Rank = Two | Three | Four | Five | Six | Seven
          | Eight | Nine | Ten | Jack | Queen 
          | King | Ace
                   
data Suit = Heart | Diamond | Club | Spade

newtype Card = Card Word64
                   
assignedPrime :: Rank -> Int
assignedPrime Two = 2
assignedPrime Three = 3
assignedPrime Four = 5
assignedPrime Five = 7 
assignedPrime Six = 11
assignedPrime Seven = 13
assignedPrime Eight = 17
assignedPrime Nine = 19
assignedPrime Ten = 23
assignedPrime Jack = 29
assignedPrime Queen = 31
assignedPrime King = 37
assignedPrime Ace = 41
