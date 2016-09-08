module Snorkels.Types ( Position
                      , Snorkel (..)
                      , Player
                      , Group (..)
                      , Piece (..)
                      , Game (..)
                      , isSnorkel
                      , getPlayer
                      , isBlocking
                      , PlayerType (..)
                      ) where

import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-- | Some (x, y) coordinate on the board
type Position = (Int, Int)


-- | Some player's pieces' color
data Snorkel = Green | Purple | Red | Yellow | Cyan
    deriving (Show, Eq, Ord, Enum, Read)


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


data PlayerType = PlayerType {
                               getMove :: Game -> Maybe String -> IO (Maybe Position)
                             , getSwitch :: Game -> Maybe String -> IO Player
                             }


data Game = Game { 
                 -- | Only 'Position's occupied by 'Piece's are here
                   pieces :: Map.Map Position Piece
                 -- | Width and height limits of the board
                 , boardSize :: (Int, Int)
                 -- | List of 'Player's that didn't 'Quit'
                 , playerTypes :: Map.Map Player PlayerType
                 -- | 'Player' that plays next
                 , currentPlayer :: Player
                 -- | Map of what each player chooses to be when asked to switch
                 , switches :: Bimap.Bimap Player Player
                 }
