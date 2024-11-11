module Calculation.GameLogic where

import Model.Card
import Model.Hand
import Model.GameState
import System.Random.Shuffle (shuffleM)
import System.Random

createDeck :: [Card]
createDeck = [Card rank suit | suit <- [minBound ..], rank <- [minBound ..]]

shuffleDeck :: [Card] -> IO [Card]
shuffleDeck deck = shuffleM deck

dealCard :: [Card] -> (Card, [Card])
dealCard (c:cs) = (c, cs)
dealCard []     = error "Cannot deal from an empty deck"

addCardToHand :: Hand -> Card -> Hand
addCardToHand hand card = hand {cards = cards hand ++ [card]}

cardValues :: CardRank -> [Int]
cardValues Ace   = [1, 11]
cardValues Two   = [2]
cardValues Three = [3]
cardValues Four  = [4]
cardValues Five  = [5]
cardValues Six   = [6]
cardValues Seven = [7]
cardValues Eight = [8]
cardValues Nine  = [9]
cardValues Ten   = [10]
cardValues Jack  = [10]
cardValues Queen = [10]
cardValues King  = [10]

possibleHandValues :: Hand -> [Int]
possibleHandValues (Hand cs) = foldl addCardValues [0] cs
  where
    addCardValues totals card =
      [total + value | total <- totals, value <- cardValues (rank card)]

calculateHandValue :: Hand -> Int
calculateHandValue hand =
  let values = filter (<= 21) (possibleHandValues hand)
   in if null values then minimum (possibleHandValues hand) else maximum values

isBust :: Hand -> Bool
isBust hand = minimum (possibleHandValues hand) > 21

isBlackjack :: Hand -> Bool
isBlackjack hand = calculateHandValue hand == 21 && length (cards hand) == 2

dealerShouldHit :: Hand -> Bool
dealerShouldHit hand = calculateHandValue hand < 17

determineWinner :: Hand -> Hand -> RoundState
determineWinner playerHand dealerHand =
  let playerValue = calculateHandValue playerHand
      dealerValue = calculateHandValue dealerHand
   in case () of
        _ | playerValue > 21 -> PlayerBust
          | dealerValue > 21 -> DealerBust
          | playerValue == dealerValue -> Draw
          | playerValue > dealerValue -> PlayerWin
          | otherwise -> DealerWin
