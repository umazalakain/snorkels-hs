module Snorkels.Game ( Action (..)
                     , doAction
                     , move
                     , getSurvivors
                     , getNextPlayer
                     , getWinner
                     ) where

import Control.Lens
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Snorkels.Types
import qualified Snorkels.Board as B


getSurvivors :: Game -> [Player]
getSurvivors game = case filter hasSurvived $ game^.players of
                         [] -> [game^.currentPlayer]
                         s -> s
                    where hasSurvived = (not . (B.hasLost game))


getNextPlayer :: Game -> Maybe Player
getNextPlayer game = listToMaybe
                   . drop 1
                   . dropWhile (/= game^.currentPlayer)
                   $ cycle
                   $ getSurvivors game


move :: Position -> Game -> Either String Game
move pos game
    | not validPosition = Left "Cannot place a snorkel there."
    | not survivors = Left "No surviving players left."
    | otherwise = Right $ nextPlayer . putSnorkel $ game
    where validPosition = elem pos $ B.freePositions game
          survivors = isJust $ getNextPlayer game
          putSnorkel g = B.putPiece g pos $ Snorkel (game^.currentPlayer)
          nextPlayer g = g & currentPlayer .~ (fromJust $ getNextPlayer g)


getWinner :: Game -> Maybe Player
getWinner game = case getSurvivors game of
                      (x:[]) -> Just x
                      _ -> Nothing




data Action = Move Position | Switch Player | Quit


doAction :: Action -> Game -> Either String Game
doAction (Move pos) game = move pos game
-- TODO: Define for switch
-- TODO: Define for quit


{-getNumberOfMoves :: Game -> Int-}
{-getNumberOfMoves -}
