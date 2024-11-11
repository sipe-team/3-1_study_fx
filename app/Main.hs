module Main (main) where

import Calculation.GameLogic
import Action.IOActions
import Model.Card
import Model.Hand
import Model.GameState

main :: IO ()
main = do
  putStrLn "블랙잭 게임에 오신 것을 환영합니다!"
  gameLoop
  putStrLn "게임을 종료합니다. 감사합니다!"

gameLoop :: IO ()
gameLoop = do
  initialGameState <- initializeGameState
  finalGameState <- playRound initialGameState
  displayResult (roundState finalGameState)
  continuationAction <- getGameContinuationAction
  case continuationAction of
    Continue -> gameLoop
    Exit     -> return ()

initializeGameState :: IO GameState
initializeGameState = do
  deck <- shuffleDeck createDeck
  let (playerHand, dealerHand, remainingDeck) = dealInitialHands deck
  return
    GameState
      { deck = remainingDeck
      , playerHand = playerHand
      , dealerHand = dealerHand
      , roundState = Ongoing
      }

dealInitialHands :: [Card] -> (Hand, Hand, [Card])
dealInitialHands deck =
  let (playerCard1, deck1) = dealCard deck
      (dealerCard1, deck2) = dealCard deck1
      (playerCard2, deck3) = dealCard deck2
      (dealerCard2, deck4) = dealCard deck3
      playerHand = Hand [playerCard1, playerCard2]
      dealerHand = Hand [dealerCard1, dealerCard2]
   in (playerHand, dealerHand, deck4)

playRound :: GameState -> IO GameState
playRound gameState = do
  let updatedGameState = checkInitialBlackjack gameState
  case roundState updatedGameState of
    PlayerBlackjack -> do
      displayGameState
        (playerHand updatedGameState)
        (dealerHand updatedGameState)
        False
      return updatedGameState
    DealerBlackjack -> do
      displayGameState
        (playerHand updatedGameState)
        (dealerHand updatedGameState)
        False
      return updatedGameState
    Draw -> do
      displayGameState
        (playerHand updatedGameState)
        (dealerHand updatedGameState)
        False
      return updatedGameState
    _ -> do
      afterPlayerTurn <- playerTurn updatedGameState
      if roundState afterPlayerTurn == PlayerBust
        then do
          displayGameState
            (playerHand afterPlayerTurn)
            (dealerHand afterPlayerTurn)
            False
          return afterPlayerTurn
        else do
          afterDealerTurn <- dealerTurn afterPlayerTurn
          let finalRoundState =
                determineWinner
                  (playerHand afterDealerTurn)
                  (dealerHand afterDealerTurn)
              finalGameState =
                afterDealerTurn {roundState = finalRoundState}
          displayGameState
            (playerHand finalGameState)
            (dealerHand finalGameState)
            False
          return finalGameState

checkInitialBlackjack :: GameState -> GameState
checkInitialBlackjack gameState =
  let playerBlackjack = isBlackjack (playerHand gameState)
      dealerBlackjack = isBlackjack (dealerHand gameState)
      newRoundState =
        case () of
          _ | playerBlackjack && dealerBlackjack -> Draw
            | playerBlackjack -> PlayerBlackjack
            | dealerBlackjack -> DealerBlackjack
            | otherwise -> roundState gameState
   in gameState {roundState = newRoundState}

playerTurn :: GameState -> IO GameState
playerTurn gameState = do
  displayGameState (playerHand gameState) (dealerHand gameState) True
  action <- getPlayerAction
  case action of
    Hit -> do
      let (newCard, newDeck) = dealCard (deck gameState)
          newPlayerHand = addCardToHand (playerHand gameState) newCard
          newGameState =
            gameState {deck = newDeck, playerHand = newPlayerHand}
      if isBust newPlayerHand
        then return newGameState {roundState = PlayerBust}
        else playerTurn newGameState
    Stand -> return gameState {roundState = PlayerStand}

dealerTurn :: GameState -> IO GameState
dealerTurn gameState = do
  displayGameState (playerHand gameState) (dealerHand gameState) False
  if dealerShouldHit (dealerHand gameState)
    then do
      putStrLn "딜러가 카드를 받습니다."
      let (newCard, newDeck) = dealCard (deck gameState)
          newDealerHand = addCardToHand (dealerHand gameState) newCard
          newGameState =
            gameState {deck = newDeck, dealerHand = newDealerHand}
      if isBust newDealerHand
        then return newGameState {roundState = DealerBust}
        else dealerTurn newGameState
    else return gameState {roundState = DealerStand}
