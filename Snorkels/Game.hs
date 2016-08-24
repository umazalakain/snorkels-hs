module Snorkels.Game ( isValidMove
                     , move
                     ) where

import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Snorkels.Types
import qualified Snorkels.Board as B


isValidMove :: Position -> Game -> Bool
isValidMove pos game = B.isValid (game^.board) pos && Map.notMember pos (game^.board.pieces)


nextPlayer :: Game -> Game
nextPlayer game = game & currentPlayer .~ nextPlayer
                   where ps = game^.players
                         nextPlayer = head
                                    . drop 1
                                    . dropWhile (/= game^.currentPlayer)
                                    $ ps ++ (take 1 ps)


move :: Position -> Game -> Game
move pos game
        | isValidMove pos game = nextPlayer $ game & board .~ (B.putPiece (game^.board) pos piece)
        | otherwise = game
        where piece = Snorkel (game^.currentPlayer)
