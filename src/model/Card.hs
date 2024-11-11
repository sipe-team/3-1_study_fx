module Model.Card where

data Card = Card { rank :: CardRank, suit :: CardSuit } deriving (Show, Eq)

data CardRank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Bounded)

data CardSuit = Heart | Club | Diamond | Spade deriving (Show, Eq, Enum, Bounded)