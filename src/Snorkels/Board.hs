module Snorkels.Board ( Position
                      , Snorkel (..)
                      , Player
                      , Group (..)
                      , Piece (..)
                      , Board (..)
                      , isSnorkel
                      , getPlayer
                      , isBlocking
                      -- * Checkers
                      , isValid
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
                      , shufflePositions
                      , throwStones
                      ) where

import Control.Monad (mfilter)
import Data.Function
import Data.Maybe
import System.Random (RandomGen, randomRs)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-- | Some (x, y) coordinate on the board
type Position = (Int, Int)


-- | Some player's pieces' color
data Snorkel = Green | Purple | Red | Yellow | Cyan
    deriving (Show, Eq, Ord, Enum)


-- | Each player has a distinctive color
type Player = Snorkel


-- |
-- An horizontally or vertically connected group of 'Snorkel's that belong to
-- the same 'Player'
data Group = Group { positions :: Set.Set Position
                   , player :: Player
                   } deriving (Show, Eq, Ord)


-- |
-- Any type of piece on the board: either a 'Snorkel' or a 'Stone'
data Piece = Snorkel Snorkel | Stone
    deriving (Show, Eq)


data Board = Board {
    -- | Only 'Position's occupied by 'Piece's are here
      pieces :: Map.Map Position Piece
    -- | Width and height limits of the board
    , size :: (Int, Int)
} deriving (Eq)


-- This is just a shortcut for (isJust . getPlayer)
-- | Check whether the 'Piece' is a 'Snorkel'
isSnorkel :: Piece -> Bool
isSnorkel Stone = False
isSnorkel _ = True


-- | Get the player owning the 'Piece' or 'Nothing' if the piece is a 'Stone'
getPlayer :: Piece -> Maybe Player
getPlayer Stone = Nothing
getPlayer (Snorkel p) = Just p


-- |
-- Check whether the contents of a given 'Position' on the board suppose a block
-- for a given 'Player'. Only 'Stone's and 'Snorkel's from a different 'Player'
-- suppose a block.
isBlocking :: Player -> Maybe Piece -> Bool
isBlocking _ Nothing = False
isBlocking player (Just piece) = maybe True (/= player) (getPlayer piece)


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
neighbours position = Set.map (`offset` position) neighbourOffsets
    where neighbourOffsets = Set.fromList [(-1, 0), (1, 0), (0, -1), (0, 1)]

-- |
-- Check if some 'Position' is within the bounds of a board
isValid :: Board -> Position -> Bool
isValid board = inBounds $ board&size

-- |
-- Check if some 'Position's are within the bounds of a board
areValid :: Board -> Set.Set Position -> Set.Set Position
areValid board = Set.filter (isValid board)

-- |
-- Get all the 'Position's that are within a board
allPositions :: Board -> Set.Set Position
allPositions board = Set.fromList [(x, y) | x <- [0..width-1], y <- [0..height-1]]
    where (width, height) = board&size

-- |
-- Get all the 'Position's that are within a board and that haven't been already
-- occupied.
freePositions :: Board -> Set.Set Position
freePositions board = Set.filter (flip Map.notMember $ board&pieces)
                    . allPositions
                    $ board

-- |
-- Get all the neighbour 'Position's of some 'Position's within a board.
areNeighbours :: Board -> Set.Set Position -> Set.Set Position
areNeighbours board positions = areValid board
                              . flip Set.difference positions
                              . Set.unions
                              . map neighbours
                              $ Set.toList positions

-- |
-- Filter 'Position's only leaving those which have a 'Piece' in some board.
arePieces :: Board -> Set.Set Position -> Set.Set Position
arePieces board = Set.intersection (Map.keysSet (board&pieces)) . areValid board

-- |
-- Filter 'Position's only leaving those which have a 'Snorkel' in some board.
areSnorkels :: Board -> Set.Set Position -> Set.Set Position
areSnorkels board = Set.filter (maybe False isSnorkel . getPiece board) . arePieces board

-- |
-- Filter 'Position's only leaving those which have a 'Snorkel' of the given
-- 'Player' in some board.
areFromPlayer :: Board -> Player -> Set.Set Position -> Set.Set Position
areFromPlayer board player = Set.filter (maybe False fromPlayer . getPiece board)
                           . areSnorkels board
    where fromPlayer = maybe False (player ==) . getPlayer

-- |
-- Put into a 'Group' 'Position's that are immediate neighbours.
growGroup :: Board -> Group -> Group
growGroup board initial
            | Set.null new = initial
            | otherwise = growGroup board group
        where group = Group {positions = Set.union initialPositions new, player = owner}
              new = areFromPlayer board owner $ areNeighbours board initialPositions
              initialPositions = initial&positions
              owner = initial&player

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
    where groupForPlayer p = Group {positions = Set.singleton pos, player = p}
          owner = mfilter isSnorkel (getPiece board pos) >>= getPlayer

-- |
-- Get all the 'Group's on the board.
getGroups :: Board -> Set.Set Group
getGroups board = Set.map fromJust
                . Set.filter isJust
                . Set.map (groupFrom board)
                $ allPositions board

-- |
-- Check whether a given 'Group' is trapped by having all its surrounding
-- positions taken by 'Stone's or 'Snorkel's from some other 'Player'.
isTrapped :: Board -> Group -> Bool
isTrapped board group = all (isBlocking (group&player) . getPiece board)
                            (Set.toList $ areNeighbours board $ group&positions)

-- |
-- Check whether the given 'Player' has one of its snorkel 'Group's trapped.
hasLost :: Board -> Player -> Bool
hasLost board p = any (isTrapped board)
                      (filter ((== p) . player) $ Set.toList $ getGroups board)


-- |
-- Get whatever is at the given 'Position' on the board.
getPiece :: Board -> Position -> Maybe Piece
getPiece board pos = Map.lookup pos $ board&pieces


-- |
-- Put a 'Piece' at the given 'Position' on the board.
putPiece :: Board -> Position -> Piece -> Board
putPiece board pos piece = board {pieces = Map.insert pos piece $ board&pieces}


-- |
-- Given a 'Set.Set' of 'Position's, get them in an ordered random list.
shufflePositions :: RandomGen g => Set.Set Position -> g -> [Position]
shufflePositions positions g = map (p !!) $ randomRs (0, length p - 1) g
    where p = Set.toList positions


throwStone :: RandomGen g => Board -> g -> Either String Board
throwStone board g
    | null $ freePositions board = Left "There is no place to throw a stone."
    | otherwise = Right $ putPiece board pos Stone
    where pos = head $ shufflePositions (freePositions board) g

-- |
-- Randomly throw the given number of 'Stone's on the board.
throwStones :: RandomGen g => Board -> Int -> g -> Either String Board
throwStones board 0 _ = Right board
throwStones board n g = case throwStone board g of
                            Right board -> throwStones board (n-1) g
                            Left message -> Left message
