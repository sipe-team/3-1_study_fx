{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Data.List (intercalate)
import System.Random (randomRIO)

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)

data Rank = Numeric Int | Jack | Queen | King | Ace deriving (Show, Eq)

data Card = Card {rank :: Rank, suit :: Suit} deriving (Show, Eq)

type Deck = [Card]

type Hand = [Card]

main :: IO ()
main = do
  putStrLn "Welcome to the 21 Card Game!"
  gameLoop

-- 게임 진행 루프
gameLoop :: IO ()
gameLoop = do
  deck <- shuffleDeck fullDeck
  let (playerHand, deck') = drawCards 2 deck
      (dealerHand, deck'') = drawCards 2 deck'
  putStrLn "Starting a new round..."
  playRound playerHand dealerHand deck''

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

-- 카드 나누기
drawCards :: Int -> Deck -> (Hand, Deck)
drawCards n deck = splitAt n deck

-- 카드 점수 계산
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

-- 플레이어의 턴
playerTurn :: Hand -> Hand -> Deck -> IO ()
playerTurn playerHand dealerHand deck = do
  putStrLn $ "Your hand: " ++ showHand playerHand ++ " (Total: " ++ show (handValue playerHand) ++ ")"
  putStrLn $ "Dealer's visible card: " ++ showCard (head dealerHand)
  if handValue playerHand == 21
    then do
      putStrLn "Blackjack! You win!"
    else
      if handValue playerHand > 21
        then do
          putStrLn "Bust! You exceeded 21. Dealer wins."
        else do
          putStrLn "Do you want to 'hit' or 'stand'?"
          action <- getLine
          case action of
            "hit" -> do
              let (newCard : deck') = deck
                  newHand = newCard : playerHand
              playerTurn newHand dealerHand deck'
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
    then
      putStrLn "Dealer busts! You win!"
    else
      if dealerScore >= 17
        then
          compareHands playerHand dealerHand
        else do
          putStrLn "Dealer hits."
          let (newCard : deck') = deck
              newHand = newCard : dealerHand
          dealerTurn playerHand newHand deck'

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

-- 카드 출력 형식
showCard :: Card -> String
showCard (Card rank suit) = show rank ++ " of " ++ show suit

showHand :: Hand -> String
showHand hand = intercalate ", " (map showCard hand)

-- 라운드 진행
playRound :: Hand -> Hand -> Deck -> IO ()
playRound playerHand dealerHand deck = do
  if handValue playerHand == 21 && handValue dealerHand == 21
    then
      putStrLn "Both you and the dealer have Blackjack! It's a tie!"
    else
      if handValue playerHand == 21
        then
          putStrLn "Blackjack! You win!"
        else
          if handValue dealerHand == 21
            then
              putStrLn "Dealer has Blackjack! You lose."
            else
              playerTurn playerHand dealerHand deck
