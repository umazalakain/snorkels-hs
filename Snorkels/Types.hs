module Snorkels.Types ( Position
                      , Snorkel (..)
                      , Player
                      , Group (..)
                      , Piece (..)
                      , Board (..)
                      , isSnorkel
                      , getPlayer
                      , isBlocking
                      ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


type Position = (Int, Int)

data Snorkel = Green | Purple
    deriving (Show, Eq, Ord)

type Player = Snorkel

data Group = Group { positions :: Set.Set Position
                   , player :: Player
                   } deriving (Show, Eq, Ord)

data Piece = Snorkel Snorkel | Stone
    deriving (Show, Eq)


-- This is just a shortcut for (isJust . getPlayer)
isSnorkel :: Piece -> Bool
isSnorkel Stone = False
isSnorkel _ = True


getPlayer :: Piece -> Maybe Player
getPlayer Stone = Nothing
getPlayer (Snorkel p) = Just p


isBlocking :: Player -> Maybe Piece -> Bool
isBlocking _ Nothing = False
isBlocking player (Just piece) = maybe True (/= player) (getPlayer piece)


data Board = Board { pieces :: Map.Map Position Piece
                   , size :: (Int, Int)
                   , players :: [Player]
                   , currentPlayer :: Player
                   } deriving (Eq)


