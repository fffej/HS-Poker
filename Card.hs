module Card (
    Suit(..),
    Value(..),
    Card(..),
    getSuit,
    getValue,
    cc
  ) where

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

data Card = Card Suit Value deriving Eq

getSuit :: Card -> Suit
getSuit (Card x _) = x

getValue :: Card -> Value
getValue (Card _ v) = v

instance Show Card where
  show (Card suit value) = show value ++ " of " ++ show suit
