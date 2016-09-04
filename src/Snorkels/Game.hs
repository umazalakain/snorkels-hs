module Snorkels.Game ( isValidMove
                     , move
                     , getSurvivors
                     , getWinner
                     ) where

import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Snorkels.Types
import qualified Snorkels.Board as B


isValidMove :: Position -> Game -> Bool
isValidMove pos game = elem pos $ B.freePositions game


getSurvivors :: Game -> [Player]
getSurvivors game = case filter hasSurvived $ game^.players of
                         [] -> [game^.currentPlayer]
                         s -> s
                    where hasSurvived = (not . (B.hasLost game))


nextPlayer :: Game -> Game
nextPlayer game = game & currentPlayer .~ nextPlayer
                   where survivors = getSurvivors game
                         nextPlayer = head
                                    . drop 1
                                    . dropWhile (/= game^.currentPlayer)
                                    $ cycle survivors


move :: Position -> Game -> Game
move pos game
        | isValidMove pos game = nextPlayer $ B.putPiece game pos piece
        | otherwise = game
        where piece = Snorkel (game^.currentPlayer)


getWinner :: Game -> Maybe Player
getWinner game = case getSurvivors game of
                      (x:[]) -> Just x
                      _ -> Nothing
