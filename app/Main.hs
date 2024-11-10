import Decks (drawCards, newDeck)
import InGame (playRound)

main :: IO ()
main = do
  putStrLn "Welcome to the 21 Card Game!"
  gameLoop

-- 게임 진행 루프
gameLoop :: IO ()
gameLoop = do
  deck <- newDeck
  let (playerHand, deck') = drawCards 2 deck
      (dealerHand, deck'') = drawCards 2 deck'
  putStrLn "Starting a new round..."
  playRound playerHand dealerHand deck''