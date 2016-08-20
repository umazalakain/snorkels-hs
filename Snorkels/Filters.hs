module Snorkels.Filters ( areValid
                        , allPositions
                        , areNeighbours
                        , arePieces
                        , areSnorkels
                        , areFromPlayer
                        ) where

import Snorkels.Types

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


inRange :: (Int, Int) -> Int -> Bool
inRange (min, max) check = min <= check && check < max

inBounds :: (Int, Int) -> Position -> Bool
inBounds (maxX, maxY) (x, y) = inRange (0, maxX) x && inRange (0, maxY) y

offset :: (Int, Int) -> Position -> Position
offset (x, y) (x2, y2) = (x+x2, y+y2)

neighbours :: Position -> Set.Set Position
neighbours position = Set.map (flip offset position) neighbourOffsets
                      where neighbourOffsets = Set.fromList [(-1, 0), (1, 0), (0, -1), (0, 1)]

areValid :: Board -> Set.Set Position -> Set.Set Position
areValid board = Set.filter (inBounds (size board))

allPositions :: Board -> Set.Set Position
allPositions board = Set.fromList [(x, y) | x <- [0..width], y <- [0..height]]
                     where (width, height) = (size board)

areNeighbours :: Board -> Set.Set Position -> Set.Set Position
areNeighbours board positions = (areValid board) . (Set.difference positions) . (Set.unions) . (map neighbours) $ Set.toList positions

arePieces :: Board -> Set.Set Position -> Set.Set Position
arePieces board = Set.union (Map.keysSet (pieces board)) . (areValid board)

isSnorkel :: Piece -> Bool
isSnorkel Stone = False
isSnorkel _ = True

areSnorkels :: Board -> Set.Set Position -> Set.Set Position
areSnorkels board = Set.filter (isSnorkel . getPiece) . (arePieces board)
                    where getPiece = (pieces board Map.!)

areFromPlayer :: Board -> Player -> Set.Set Position -> Set.Set Position
areFromPlayer board player = Set.filter (fromPlayer . getPiece) . (areSnorkels board)
                             where getPiece = (pieces board Map.!)
                                   fromPlayer = (== (Snorkel player))
