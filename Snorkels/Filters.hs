module Snorkels.Filters ( areValid
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

areValid :: Board -> Set.Set Position -> Set.Set Position
areValid board = Set.filter (inBounds (size board))

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
