module Snorkels.Board ( areValid
                      , isValid
                      , allPositions
                      , freePositions
                      , areNeighbours
                      , arePieces
                      , areSnorkels
                      , areFromPlayer
                      , growGroup
                      , groupFrom
                      , getGroups
                      , isTrapped
                      , hasLost
                      , getPiece
                      , putPiece
                      , throwStones
                      ) where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Functor
import System.Random
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
isValid board = inBounds $ board^.size

areValid :: Board -> Set.Set Position -> Set.Set Position
areValid board = Set.filter (isValid board)

allPositions :: Board -> Set.Set Position
allPositions board = Set.fromList [(x, y) | x <- [0..width-1], y <- [0..height-1]]
                     where (width, height) = board^.size

freePositions :: Board -> Set.Set Position
freePositions board = Set.filter (flip Map.notMember $ board^.pieces)
                    . allPositions
                    $ board

areNeighbours :: Board -> Set.Set Position -> Set.Set Position
areNeighbours board positions = areValid board
                              . flip Set.difference positions
                              . Set.unions
                              . map neighbours
                              $ Set.toList positions

arePieces :: Board -> Set.Set Position -> Set.Set Position
arePieces board = Set.intersection (Map.keysSet (board^.pieces)) . areValid board

areSnorkels :: Board -> Set.Set Position -> Set.Set Position
areSnorkels board = Set.filter (maybe False isSnorkel . getPiece board) . arePieces board

areFromPlayer :: Board -> Player -> Set.Set Position -> Set.Set Position
areFromPlayer board player = Set.filter (maybe False fromPlayer . getPiece board) . areSnorkels board
                             where fromPlayer = (maybe False (player ==) . getPlayer)

growGroup :: Board -> Group -> Group
growGroup board initial
            | Set.null new = initial
            | otherwise = growGroup board group
            where group = Group {_positions = (Set.union initialPositions new), _player = owner}
                  new = areFromPlayer board owner $ areNeighbours board initialPositions
                  initialPositions = initial^.positions
                  owner = initial^.player

-- TODO: Should this return a Maybe Group (to account for the possibility of the
-- given position on the board being empty) or allow groups of empty positions
-- too? Such groups might be useful for AI if we ever dare go there.
groupFrom :: Board -> Position -> Maybe Group
groupFrom board pos = growGroup board <$> (groupForPlayer <$> owner)
                      where groupForPlayer = \p -> Group {_positions = Set.singleton pos, _player = p}
                            owner = (mfilter isSnorkel $ getPiece board pos) >>= getPlayer

getGroups :: Board -> Set.Set Group
getGroups board = Set.map fromJust
                . Set.filter isJust
                . Set.map (groupFrom board)
                $ allPositions board

isTrapped :: Board -> Group -> Bool
isTrapped board group = and
                      . map (isBlocking (group^.player) . (getPiece board))
                      . Set.toList
                      . areNeighbours board
                      $ group^.positions

hasLost :: Board -> Player -> Bool
hasLost board p = or
                . map (isTrapped board)
                . filter (^.player.to (== p))
                . Set.toList
                $ getGroups board


getPiece :: Board -> Position -> Maybe Piece
getPiece board pos = Map.lookup pos $ board^.pieces


putPiece :: Board -> Position -> Piece -> Board
putPiece board pos piece = board & pieces .~ (board^.pieces & at pos ?~ piece)


shufflePositions :: RandomGen g => Set.Set Position -> g -> [Position]
shufflePositions positions g = map (p !!) $ randomRs (0, length p - 1) g
                               where p = Set.toList positions


throwStones :: RandomGen g => Board -> Int -> g -> Board
throwStones board n g = foldl throwStone board $ take n $ shufflePositions (freePositions board) g 
                        where throwStone b p = putPiece b p Stone
