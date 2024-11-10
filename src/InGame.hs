module InGame (playRound) where

import Decks (Deck, Hand, handValue, showCard)
import Data.List (intercalate)
import Data.Char (toLower)
import System.IO (hFlush, stdout)

-- 플레이어의 턴
playerTurn :: Hand -> Hand -> Deck -> IO ()
playerTurn playerHand dealerHand deck = do
  putStrLn $ "Your hand: " ++ showHand playerHand ++ " (Total: " ++ show (handValue playerHand) ++ ")"
  putStrLn $ "Dealer's visible card: " ++ showCard (head dealerHand)
  let playerScore = handValue playerHand
  if playerScore == 21
    then putStrLn "Blackjack! You win!"
    else if playerScore > 21
      then putStrLn "Bust! You exceeded 21. Dealer wins."
      else do
        putStr "Do you want to 'hit' or 'stand'? "
        hFlush stdout  -- Ensures the prompt is displayed before input
        action <- map toLower <$> getLine
        case action of
          "hit" -> case deck of
            (newCard : deck') -> do
              let newHand = newCard : playerHand
              playerTurn newHand dealerHand deck'
            [] -> putStrLn "Deck is empty. Unable to draw a card."
          "stand" -> dealerTurn playerHand dealerHand deck
          _ -> do
            putStrLn "Invalid choice. Please type 'hit' or 'stand'."
            playerTurn playerHand dealerHand deck

-- 딜러의 턴
dealerTurn :: Hand -> Hand -> Deck -> IO ()
dealerTurn playerHand dealerHand deck = do
  let dealerScore = handValue dealerHand
  putStrLn $ "Dealer's hand: " ++ showHand dealerHand ++ " (Total: " ++ show dealerScore ++ ")"
  if dealerScore > 21
    then putStrLn "Dealer busts! You win!"
    else if dealerScore >= 17
      then compareHands playerHand dealerHand
      else case deck of
        (newCard : deck') -> do
          putStrLn "Dealer hits."
          let newHand = newCard : dealerHand
          dealerTurn playerHand newHand deck'
        [] -> putStrLn "Deck is empty. Dealer cannot draw a card."

-- 결과 비교
compareHands :: Hand -> Hand -> IO ()
compareHands playerHand dealerHand = do
  let playerScore = handValue playerHand
      dealerScore = handValue dealerHand
  putStrLn $ "Your total: " ++ show playerScore
  putStrLn $ "Dealer's total: " ++ show dealerScore
  case compare playerScore dealerScore of
    GT -> putStrLn "You win!"
    LT -> putStrLn "Dealer wins!"
    EQ -> putStrLn "It's a tie!"

-- 핸드 출력
showHand :: Hand -> String
showHand hand = intercalate ", " (map showCard hand)

-- 라운드 진행
playRound :: Hand -> Hand -> Deck -> IO ()
playRound playerHand dealerHand deck = do
  let playerScore = handValue playerHand
      dealerScore = handValue dealerHand
  if playerScore == 21 && dealerScore == 21
    then putStrLn "Both you and the dealer have Blackjack! It's a tie!"
    else if playerScore == 21
      then putStrLn "Blackjack! You win!"
      else if dealerScore == 21
        then putStrLn "Dealer has Blackjack! You lose."
        else playerTurn playerHand dealerHand deck