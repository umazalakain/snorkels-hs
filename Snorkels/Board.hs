module Snorkels.Board ( areValid
                      , isValid
                      , allPositions
                      , areNeighbours
                      , arePieces
                      , areSnorkels
                      , areFromPlayer
                      , growGroup
                      , groupFrom
                      , getGroups
                      , isTrapped
                      , hasLost
                      , move
                      ) where

import Control.Monad
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Snorkels.Types


inRange :: (Int, Int) -> Int -> Bool
inRange (min, max) check = min <= check && check < max

inBounds :: (Int, Int) -> Position -> Bool
inBounds (maxX, maxY) (x, y) = inRange (0, maxX) x && inRange (0, maxY) y

offset :: (Int, Int) -> Position -> Position
offset (x, y) (x2, y2) = (x+x2, y+y2)

neighbours :: Position -> Set.Set Position
neighbours position = Set.map (flip offset position) neighbourOffsets
                      where neighbourOffsets = Set.fromList [(-1, 0), (1, 0), (0, -1), (0, 1)]

isValid :: Board -> Position -> Bool
isValid board = inBounds $ size board

areValid :: Board -> Set.Set Position -> Set.Set Position
areValid board = Set.filter (isValid board)

allPositions :: Board -> Set.Set Position
allPositions board = Set.fromList [(x, y) | x <- [0..width], y <- [0..height]]
                     where (width, height) = (size board)

areNeighbours :: Board -> Set.Set Position -> Set.Set Position
areNeighbours board positions = areValid board
                              . flip Set.difference positions
                              . Set.unions
                              . map neighbours
                              $ Set.toList positions

arePieces :: Board -> Set.Set Position -> Set.Set Position
arePieces board = Set.intersection (Map.keysSet (pieces board)) . areValid board

areSnorkels :: Board -> Set.Set Position -> Set.Set Position
areSnorkels board = Set.filter (maybe False isSnorkel . getPiece board) . arePieces board

areFromPlayer :: Board -> Player -> Set.Set Position -> Set.Set Position
areFromPlayer board player = Set.filter (maybe False fromPlayer . getPiece board) . areSnorkels board
                             where fromPlayer = (maybe False (player ==) . getPlayer)

growGroup :: Board -> Group -> Group
growGroup board initial
            | Set.null new = initial
            | otherwise = growGroup board group
            where group = Group {positions = (Set.union initialPositions new), player = owner}
                  new = areFromPlayer board owner $ areNeighbours board initialPositions
                  initialPositions = positions initial
                  owner = player initial

-- TODO: Should this return a Maybe Group (to account for the possibility of the
-- given position on the board being empty) or allow groups of empty positions
-- too? Such groups might be useful for AI if we ever dare go there.
groupFrom :: Board -> Position -> Maybe Group
groupFrom board pos = growGroup board <$> (groupForPlayer <$> owner)
                      where groupForPlayer = \p -> Group {positions = Set.singleton pos, player = p}
                            owner = (mfilter isSnorkel $ getPiece board pos) >>= getPlayer

getGroups :: Board -> Set.Set Group
getGroups board = Set.map fromJust
                . Set.filter isJust
                . Set.map (groupFrom board)
                $ allPositions board

isTrapped :: Board -> Group -> Bool
isTrapped board group = and
                      . map (isBlocking (player group) . (getPiece board))
                      . Set.toList
                      . areNeighbours board
                      . positions
                      $ group

hasLost :: Board -> Player -> Bool
hasLost board p = or
                . map (isTrapped board)
                . filter ((== p) . player)
                . Set.toList
                $ getGroups board


isValidMove :: Position -> Board -> Bool
isValidMove pos board = isValid board pos && Map.notMember pos (pieces board)


nextPlayer :: Board -> Board
nextPlayer board = board { currentPlayer = nextPlayer }
                   where ps = players board
                         nextPlayer = head
                                    . drop 1
                                    . dropWhile (/= currentPlayer board)
                                    $ ps ++ (take 1 ps)


getPiece :: Board -> Position -> Maybe Piece
getPiece board pos = Map.lookup pos (pieces board)


putPiece :: Board -> Position -> Piece -> Board
putPiece board pos piece = board { pieces = Map.insert pos piece $ pieces board }


move :: Position -> Board -> Board
move pos board
        | isValidMove pos board = nextPlayer $ putPiece board pos piece
        | otherwise = board
        where piece = Snorkel (currentPlayer board)
