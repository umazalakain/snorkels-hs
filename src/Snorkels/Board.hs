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


-- | 
-- Given some @(min, max)@ bounds, check if an 'Int' is in them.
-- @min@ is inclusive, @max@ isn't.
inRange :: (Int, Int) -> Int -> Bool
inRange (min, max) check = min <= check && check < max

-- |
-- Given some @(maxWidth, maxHeight)@ bounds and a 'Position', check whether the
-- 'Position' is within those bounds.
inBounds :: (Int, Int) -> Position -> Bool
inBounds (maxX, maxY) (x, y) = inRange (0, maxX) x && inRange (0, maxY) y

-- |
-- Offset by @(x, y)@ a 'Position' to obtain a new one
offset :: (Int, Int) -> Position -> Position
offset (x, y) (x2, y2) = (x+x2, y+y2)

-- |
-- Given a 'Position', get the 'Set.Set' of 'Position's that are immediatelly
-- above, under, to the left, or to the right.
neighbours :: Position -> Set.Set Position
neighbours position = Set.map (flip offset position) neighbourOffsets
                      where neighbourOffsets = Set.fromList [(-1, 0), (1, 0), (0, -1), (0, 1)]

-- | 
-- Check if some 'Position' is within the bounds of a 'Board'                            
isValid :: Board -> Position -> Bool
isValid board = inBounds $ board^.size

-- | 
-- Check if some 'Position's are within the bounds of a 'Board'                            
areValid :: Board -> Set.Set Position -> Set.Set Position
areValid board = Set.filter (isValid board)

-- | 
-- Get all the 'Position's that are within a 'Board'
allPositions :: Board -> Set.Set Position
allPositions board = Set.fromList [(x, y) | x <- [0..width-1], y <- [0..height-1]]
                     where (width, height) = board^.size

-- |
-- Get all the 'Position's that are within a 'Board' and that haven't been
-- already occupied.
freePositions :: Board -> Set.Set Position
freePositions board = Set.filter (flip Map.notMember $ board^.pieces)
                    . allPositions
                    $ board

-- |
-- Get all the neighbour 'Position's of some 'Position's within a 'Board'.
areNeighbours :: Board -> Set.Set Position -> Set.Set Position
areNeighbours board positions = areValid board
                              . flip Set.difference positions
                              . Set.unions
                              . map neighbours
                              $ Set.toList positions

-- |
-- Filter 'Position's only leaving those which have a 'Piece' in some 'Board'.
arePieces :: Board -> Set.Set Position -> Set.Set Position
arePieces board = Set.intersection (Map.keysSet (board^.pieces)) . areValid board

-- |
-- Filter 'Position's only leaving those which have a 'Snorkel' in some 'Board'
areSnorkels :: Board -> Set.Set Position -> Set.Set Position
areSnorkels board = Set.filter (maybe False isSnorkel . getPiece board) . arePieces board

-- |
-- Filter 'Position's only leaving those which have a 'Snorkel' of the given
-- 'Player' in some 'Board'.
areFromPlayer :: Board -> Player -> Set.Set Position -> Set.Set Position
areFromPlayer board player = Set.filter (maybe False fromPlayer . getPiece board) . areSnorkels board
                             where fromPlayer = (maybe False (player ==) . getPlayer)

-- |
-- Put into a 'Group' 'Position's that are immediate neighbours.
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
-- |
-- Form a 'Group' from an initial 'Position' putting all the 'Position's with
-- 'Snorkel's from the same 'Player' that are connected, vertically or
-- horizontally, and by 'Snorkel's of the same 'Player', to the initial
-- 'Position'
groupFrom :: Board -> Position -> Maybe Group
groupFrom board pos = growGroup board <$> (groupForPlayer <$> owner)
                      where groupForPlayer = \p -> Group {_positions = Set.singleton pos, _player = p}
                            owner = (mfilter isSnorkel $ getPiece board pos) >>= getPlayer

-- |
-- Get all the 'Group's on the 'Board'.
getGroups :: Board -> Set.Set Group
getGroups board = Set.map fromJust
                . Set.filter isJust
                . Set.map (groupFrom board)
                $ allPositions board

-- |
-- Check whether a given 'Group' is trapped by having all its surrounding
-- positions taken by 'Stone's or 'Snorkel's from some other 'Player'.
isTrapped :: Board -> Group -> Bool
isTrapped board group = and
                      . map (isBlocking (group^.player) . (getPiece board))
                      . Set.toList
                      . areNeighbours board
                      $ group^.positions

-- |
-- Check whether the given 'Player' has one of its snorkel 'Group's trapped.
hasLost :: Board -> Player -> Bool
hasLost board p = or
                . map (isTrapped board)
                . filter (^.player.to (== p))
                . Set.toList
                $ getGroups board


-- |
-- Get whatever is at the given 'Position' on the 'Board'.
getPiece :: Board -> Position -> Maybe Piece
getPiece board pos = Map.lookup pos $ board^.pieces


-- |
-- Put a 'Piece' at the given 'Position' on the 'Board'.
putPiece :: Board -> Position -> Piece -> Board
putPiece board pos piece = board & pieces .~ (board^.pieces & at pos ?~ piece)


-- |
-- Given a 'Set.Set' of 'Position's, get them in an ordered random list.
shufflePositions :: RandomGen g => Set.Set Position -> g -> [Position]
shufflePositions positions g = map (p !!) $ randomRs (0, length p - 1) g
                               where p = Set.toList positions


-- |
-- Randomly throw the given number of 'Stone's on the 'Board'.
throwStones :: RandomGen g => Board -> Int -> g -> Board
throwStones board n g = foldl throwStone board $ take n $ shufflePositions (freePositions board) g 
                        where throwStone b p = putPiece b p Stone
