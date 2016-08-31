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
isValidMove pos game = elem pos $ B.freePositions $ game^.board


getSurvivors :: Game -> [Player]
getSurvivors game = case filter hasSurvived $ game^.players of
                         [] -> [game^.currentPlayer]
                         s -> s
                    where hasSurvived = (not . (B.hasLost $ game^.board))


nextPlayer :: Game -> Game
nextPlayer game = game & currentPlayer .~ nextPlayer
                   where survivors = getSurvivors game
                         nextPlayer = head
                                    . drop 1
                                    . dropWhile (/= game^.currentPlayer)
                                    $ survivors ++ (take 1 survivors)


move :: Position -> Game -> Game
move pos game
        | isValidMove pos game = nextPlayer $ game & board .~ (B.putPiece (game^.board) pos piece)
        | otherwise = game
        where piece = Snorkel (game^.currentPlayer)


getWinner :: Game -> Maybe Player
getWinner game = case getSurvivors game of
                      (x:[]) -> Just x
                      _ -> Nothing
