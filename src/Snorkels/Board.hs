module Snorkels.Board ( 
                      -- * Checkers
                        isValid
                      , isTrapped
                      , hasLost
                      -- * Generators
                      , neighbours
                      , allPositions
                      , freePositions
                      , growGroup
                      , groupFrom
                      , getGroups
                      -- * Filters
                      , areValid
                      , areNeighbours
                      , arePieces
                      , areSnorkels
                      , areFromPlayer
                      -- * Manipulation
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
-- Check if some 'Position' is within the bounds of a board
isValid :: Game -> Position -> Bool
isValid game = inBounds $ game^.boardSize

-- | 
-- Check if some 'Position's are within the bounds of a game
areValid :: Game -> Set.Set Position -> Set.Set Position
areValid game = Set.filter (isValid game)

-- | 
-- Get all the 'Position's that are within a board
allPositions :: Game -> Set.Set Position
allPositions game = Set.fromList [(x, y) | x <- [0..width-1], y <- [0..height-1]]
                     where (width, height) = game^.boardSize

-- |
-- Get all the 'Position's that are within a board and that haven't been already
-- occupied.
freePositions :: Game -> Set.Set Position
freePositions game = Set.filter (flip Map.notMember $ game^.pieces)
                   . allPositions
                   $ game

-- |
-- Get all the neighbour 'Position's of some 'Position's within a board.
areNeighbours :: Game -> Set.Set Position -> Set.Set Position
areNeighbours game positions = areValid game
                             . flip Set.difference positions
                             . Set.unions
                             . map neighbours
                             $ Set.toList positions

-- |
-- Filter 'Position's only leaving those which have a 'Piece' in some board.
arePieces :: Game -> Set.Set Position -> Set.Set Position
arePieces game = Set.intersection (Map.keysSet (game^.pieces)) . areValid game

-- |
-- Filter 'Position's only leaving those which have a 'Snorkel' in some board.
areSnorkels :: Game -> Set.Set Position -> Set.Set Position
areSnorkels game = Set.filter (maybe False isSnorkel . getPiece game) . arePieces game

-- |
-- Filter 'Position's only leaving those which have a 'Snorkel' of the given
-- 'Player' in some board.
areFromPlayer :: Game -> Player -> Set.Set Position -> Set.Set Position
areFromPlayer game player = Set.filter (maybe False fromPlayer . getPiece game) . areSnorkels game
                            where fromPlayer = (maybe False (player ==) . getPlayer)

-- |
-- Put into a 'Group' 'Position's that are immediate neighbours.
growGroup :: Game -> Group -> Group
growGroup game initial
            | Set.null new = initial
            | otherwise = growGroup game group
            where group = Group {_positions = (Set.union initialPositions new), _player = owner}
                  new = areFromPlayer game owner $ areNeighbours game initialPositions
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
groupFrom :: Game -> Position -> Maybe Group
groupFrom game pos = growGroup game <$> (groupForPlayer <$> owner)
                     where groupForPlayer = \p -> Group {_positions = Set.singleton pos, _player = p}
                           owner = (mfilter isSnorkel $ getPiece game pos) >>= getPlayer

-- |
-- Get all the 'Group's on the board.
getGroups :: Game -> Set.Set Group
getGroups game = Set.map fromJust
               . Set.filter isJust
               . Set.map (groupFrom game)
               $ allPositions game

-- |
-- Check whether a given 'Group' is trapped by having all its surrounding
-- positions taken by 'Stone's or 'Snorkel's from some other 'Player'.
isTrapped :: Game -> Group -> Bool
isTrapped game group = and
                     . map (isBlocking (group^.player) . (getPiece game))
                     . Set.toList
                     . areNeighbours game
                     $ group^.positions

-- |
-- Check whether the given 'Player' has one of its snorkel 'Group's trapped.
hasLost :: Game -> Player -> Bool
hasLost game p = or
               . map (isTrapped game)
               . filter (^.player.to (== p))
               . Set.toList
               $ getGroups game


-- |
-- Get whatever is at the given 'Position' on the board.
getPiece :: Game -> Position -> Maybe Piece
getPiece game pos = Map.lookup pos $ game^.pieces


-- |
-- Put a 'Piece' at the given 'Position' on the board.
putPiece :: Game -> Position -> Piece -> Game
putPiece game pos piece = game & pieces .~ (game^.pieces & at pos ?~ piece)


-- |
-- Given a 'Set.Set' of 'Position's, get them in an ordered random list.
shufflePositions :: RandomGen g => Set.Set Position -> g -> [Position]
shufflePositions positions g = map (p !!) $ randomRs (0, length p - 1) g
                               where p = Set.toList positions


-- |
-- Randomly throw the given number of 'Stone's on the board.
throwStones :: RandomGen g => Game -> Int -> g -> Game
throwStones game n g = foldl throwStone game $ take n $ shufflePositions (freePositions game) g 
                       where throwStone b p = putPiece b p Stone
