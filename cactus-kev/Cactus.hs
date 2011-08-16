module Cactus where

import Data.Word
import Data.Bits

data Rank = Two | Three | Four | Five | Six | Seven
          | Eight | Nine | Ten | Jack | Queen 
          | King | Ace deriving (Enum,Ord,Eq,Show)
                   
data Suit = Heart | Diamond | Club | Spade deriving (Enum,Ord,Eq,Show)

newtype Card = Card Word32
                   
               
{-
   +--------+--------+--------+--------+
   |xxxbbbbb|bbbbbbbb|cdhsrrrr|xxpppppp|
   +--------+--------+--------+--------+

   p = prime number of rank (deuce=2,trey=3,four=5,...,ace=41)
   r = rank of card (deuce=0,trey=1,four=2,five=3,...,ace=12)
   cdhs = suit of card (bit turned on based on suit of card)
   b = bit turned on depending on rank of card
-}
mkCard :: Rank -> Suit -> Card
mkCard r s = Card (mkCardWord64 r s)
               
mkCardWord64 :: Rank -> Suit -> Word32
mkCardWord64 r s =  p .|. byte2 .|. byte34
  where
    cdhs = suit s
    rrrr = cardRank r
    byte2 = cdhs .|. rrrr
    p = primeRank r
    b = cardBit r
    byte34 = shiftL b 16
             
suit :: Suit -> Word32
suit Club = bit 7
suit Diamond = bit 6
suit Heart = bit 5
suit Spade = bit 4
        
cardBit :: Rank -> Word32
cardBit = bit . fromIntegral . fromEnum
              
cardRank :: Rank -> Word32
cardRank = fromIntegral . fromEnum 
                   
primeRank :: Rank -> Word32
primeRank Two = 2
primeRank Three = 3
primeRank Four = 5
primeRank Five = 7 
primeRank Six = 11
primeRank Seven = 13
primeRank Eight = 17
primeRank Nine = 19
primeRank Ten = 23
primeRank Jack = 29
primeRank Queen = 31
primeRank King = 37
primeRank Ace = 41
