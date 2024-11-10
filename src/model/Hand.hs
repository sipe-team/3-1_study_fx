module Model.Hand where

import Model.Card

data Hand = Hand { cards :: [Card] } deriving (Show, Eq)