{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Decks (Hand, Deck, newDeck, drawCards, handValue, showCard) where

import System.Random (randomRIO)

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)

data Rank = Numeric Int | Jack | Queen | King | Ace deriving (Show, Eq)

data Card = Card {rank :: Rank, suit :: Suit} deriving (Show, Eq)

type Deck = [Card]

type Hand = [Card]

-- 전체 덱 생성
fullDeck :: Deck
fullDeck = [Card rank suit | rank <- ranks, suit <- suits]
  where
    suits = [Hearts, Diamonds, Clubs, Spades]
    ranks = [Numeric n | n <- [2 .. 10]] ++ [Jack, Queen, King, Ace]

-- 카드 셔플
shuffleDeck :: Deck -> IO Deck
shuffleDeck [] = return []
shuffleDeck deck = do
  index <- randomRIO (0, length deck - 1)
  let (left, card : right) = splitAt index deck
  rest <- shuffleDeck (left ++ right)
  return (card : rest)

newDeck :: IO Deck
newDeck = shuffleDeck fullDeck

drawCards :: Int -> Deck -> (Hand, Deck)
drawCards = splitAt

cardValue :: Card -> Int
cardValue (Card (Numeric n) _) = n
cardValue (Card Ace _) = 11
cardValue _ = 10

handValue :: Hand -> Int
handValue hand =
  let baseValue = sum (map cardValue hand)
      aceCount = length (filter (\(Card r _) -> r == Ace) hand)
   in adjustForAces baseValue aceCount
  where
    adjustForAces total aceCount
      | total > 21 && aceCount > 0 = adjustForAces (total - 10) (aceCount - 1)
      | otherwise = total

showCard :: Card -> String
showCard (Card rank suit) = 
    showRank rank ++ " of " ++ show suit
  where
    showRank :: Rank -> String
    showRank (Numeric n) = show n
    showRank Jack        = "Jack"
    showRank Queen       = "Queen"
    showRank King        = "King"
    showRank Ace         = "Ace"