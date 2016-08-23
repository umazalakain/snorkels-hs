module Snorkels.Game ( isValidMove
                     , move
                     ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Snorkels.Types
import qualified Snorkels.Board as B


isValidMove :: Position -> Game -> Bool
isValidMove pos game = B.isValid (board game) pos && Map.notMember pos (pieces $ board game)


nextPlayer :: Game -> Game
nextPlayer game = game { currentPlayer = nextPlayer }
                   where ps = players game
                         nextPlayer = head
                                    . drop 1
                                    . dropWhile (/= currentPlayer game)
                                    $ ps ++ (take 1 ps)


move :: Position -> Game -> Game
move pos game
        | isValidMove pos game = nextPlayer $ game { board = B.putPiece (board game) pos piece }
        | otherwise = game
        where piece = Snorkel (currentPlayer game)
