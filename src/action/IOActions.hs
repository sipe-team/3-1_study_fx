module Action.IOActions where

import Model.Card
import Model.Hand
import Model.GameState
import Calculation.GameLogic (calculateHandValue)
import Data.Char (toLower)

data PlayerAction = Hit | Stand deriving (Show, Eq)

data GameContinuation = Continue | Exit deriving (Show, Eq)

getPlayerAction :: IO PlayerAction
getPlayerAction = do
  putStrLn "Hit 또는 Stand? (h/s): "
  input <- getLine
  case map toLower input of
    "h" -> return Hit
    "s" -> return Stand
    _   -> do
      putStrLn "h 또는 s 중 입력 해주세요"
      getPlayerAction

displayGameState :: Hand -> Hand -> Bool -> IO ()
displayGameState playerHand dealerHand hideDealerCard = do
  putStrLn $
    "\n플레이어 손패: " ++
    show (cards playerHand) ++
    " | Value: " ++ show (calculateHandValue playerHand)
  if hideDealerCard
    then putStrLn $
         "딜러 손패: [" ++ show (head (cards dealerHand)) ++ ", 알수없는 패]"
    else putStrLn $
         "딜러 손패: " ++
         show (cards dealerHand) ++
         " | Value: " ++ show (calculateHandValue dealerHand)

-- showHand :: [Card] -> String
-- showHand = unwords . map show

displayResult :: RoundState -> IO ()
displayResult state =
  case state of
    PlayerBlackjack ->
      putStrLn "\nGame Over: 플레이어 블랙잭! 승리하셨습니다!"
    DealerBlackjack ->
      putStrLn "\nGame Over: 딜러 블랙잭! 패배하셨습니다!"
    PlayerBust -> putStrLn "\nGame Over: 플레이어 버스트! 딜러의 승리입니다!"
    DealerBust -> putStrLn "\nGame Over: 딜러 버스트! 승리하셨습니다!"
    PlayerWin  -> putStrLn "\nGame Over: 승리하셨습니다!"
    DealerWin  -> putStrLn "\nGame Over: 딜러의 승리입니다!"
    Draw       -> putStrLn "\nGame Over: 무승부입니다!"
    _          -> return ()

getGameContinuationAction :: IO GameContinuation
getGameContinuationAction = do
  putStrLn "게임을 계속하시겠습니까? (y/n): "
  input <- getLine
  case map toLower input of
    "y" -> return Continue
    "n" -> return Exit
    _   -> do
      putStrLn "잘못된 입력입니다. 게임을 종료합니다."
      return Exit
