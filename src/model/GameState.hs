module Model.GameState where

import Model.Hand
import Model.Card

data RoundState
  = Ongoing
  | PlayerBust
  | DealerBust
  | PlayerBlackjack
  | DealerBlackjack
  | PlayerStand
  | DealerStand
  | Draw
  | PlayerWin
  | DealerWin
  deriving (Show, Eq)

data GameState = GameState
  { deck       :: [Card]
  , playerHand :: Hand
  , dealerHand :: Hand
  , roundState :: RoundState
  } deriving (Show, Eq)