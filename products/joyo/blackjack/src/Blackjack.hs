module Blackjack
    ( playGame
    , Card(..)
    , Suit(..)
    , Rank(..)
    ) where


import System.Random
import Control.Monad (when)
import Data.List (sortBy, intercalate)

-- 카드 타입 정의
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)
data Rank = Ace | Two | Three | Four | Five | Six | Seven 
          | Eight | Nine | Ten | Jack | Queen | King 
          deriving (Show, Eq, Enum)
data Card = Card Rank Suit deriving (Show, Eq)

-- 게임 상태
type Hand = [Card]
data GameState = GameState {
    playerHand :: Hand,
    dealerHand :: Hand,
    deck :: [Card]
} deriving Show

-- 카드 값 계산
cardValue :: Card -> Int
cardValue (Card rank _) = case rank of
    Ace -> 11
    King -> 10
    Queen -> 10
    Jack -> 10
    _ -> fromEnum rank + 1

-- 핸드의 총 값 계산 (Ace 처리 포함)
handValue :: Hand -> Int
handValue hand = 
    let total = sum $ map cardValue hand
        aces = length $ filter (\(Card rank _) -> rank == Ace) hand
    in if total > 21 && aces > 0
       then total - (10 * aces)
       else total

-- 덱 생성
fullDeck :: [Card]
fullDeck = [Card rank suit | suit <- [Hearts, Diamonds, Clubs, Spades],
                            rank <- [Ace .. King]]

-- 덱 섞기
shuffleDeck :: [Card] -> IO [Card]
shuffleDeck deck = do
    gen <- newStdGen
    return $ map snd $ sortBy (\(a,_) (b,_) -> compare a b) 
           $ zip (randoms gen :: [Float]) deck

-- 초기 게임 상태 설정
initGame :: IO GameState
initGame = do
    shuffled <- shuffleDeck fullDeck
    let (playerCards, rest1) = splitAt 2 shuffled
        (dealerCards, rest2) = splitAt 2 rest1
    return $ GameState playerCards dealerCards rest2

-- 카드 한 장 뽑기
drawCard :: GameState -> (Card, GameState)
drawCard gs@(GameState ph dh (c:cs)) = (c, gs { deck = cs })

-- 핸드 출력
showHand :: String -> Hand -> IO ()
showHand label hand = do
    putStrLn $ label ++ ": " ++ intercalate ", " (map show hand)
    putStrLn $ "Total: " ++ show (handValue hand)

-- 플레이어 턴
playerTurn :: GameState -> IO GameState
playerTurn gs = do
    showHand "Your hand" (playerHand gs)
    putStr "Hit or Stand? (h/s): "
    choice <- getLine
    case choice of
        "h" -> do
            let (card, newGs) = drawCard gs
            let newPlayerHand = card : playerHand gs
            if handValue newPlayerHand > 21
                then do
                    putStrLn "Bust!"
                    return newGs { playerHand = newPlayerHand }
                else playerTurn newGs { playerHand = newPlayerHand }
        "s" -> return gs
        _ -> playerTurn gs

-- 딜러 턴
dealerTurn :: GameState -> IO GameState
dealerTurn gs = do
    showHand "Dealer's hand" (dealerHand gs)
    if handValue (dealerHand gs) < 17
        then do
            let (card, newGs) = drawCard gs
            let newDealerHand = card : dealerHand gs
            putStrLn "Dealer hits"
            if handValue newDealerHand > 21
                then do
                    putStrLn "Dealer busts!"
                    return newGs { dealerHand = newDealerHand }
                else dealerTurn newGs { dealerHand = newDealerHand }
        else do
            putStrLn "Dealer stands"
            return gs

-- 승자 결정
determineWinner :: GameState -> IO ()
determineWinner gs = do
    let playerScore = handValue (playerHand gs)
        dealerScore = handValue (dealerHand gs)
    putStrLn $ "\nFinal scores:"
    showHand "Your hand" (playerHand gs)
    showHand "Dealer's hand" (dealerHand gs)
    if playerScore > 21 
        then putStrLn "You bust! Dealer wins!"
        else if dealerScore > 21
            then putStrLn "Dealer busts! You win!"
            else if playerScore > dealerScore
                then putStrLn "You win!"
                else if playerScore < dealerScore
                    then putStrLn "Dealer wins!"
                    else putStrLn "Push!"

-- 메인 게임 루프
playGame :: IO ()
playGame = do
    putStrLn "Welcome to Haskell Blackjack!"
    gs <- initGame
    finalPlayerState <- playerTurn gs
    if handValue (playerHand finalPlayerState) <= 21
        then do
            finalState <- dealerTurn finalPlayerState
            determineWinner finalState
        else
            determineWinner finalPlayerState
    putStr "Play again? (y/n): "
    choice <- getLine
    when (choice == "y") playGame

-- 게임 시작
main :: IO ()
main = playGame