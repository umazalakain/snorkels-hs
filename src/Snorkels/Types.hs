{-# LANGUAGE TemplateHaskell #-}

module Snorkels.Types ( Position
                      , Snorkel (..)
                      , Player
                      , Group (..)
                      , positions
                      , player
                      , Piece (..)
                      , Board (..)
                      , pieces
                      , size
                      , Game (..)
                      , board
                      , players
                      , currentPlayer
                      , history
                      , isSnorkel
                      , getPlayer
                      , isBlocking
                      ) where

import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


type Position = (Int, Int)


data Snorkel = Green | Purple | Red | Yellow | Cyan
    deriving (Show, Eq, Ord, Enum)


type Player = Snorkel


data Group = Group { _positions :: Set.Set Position
                   , _player :: Player
                   } deriving (Show, Eq, Ord)

makeLenses ''Group


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


data Board = Board { _pieces :: Map.Map Position Piece
                   , _size :: (Int, Int)
                   } deriving (Eq)

makeLenses ''Board


data Game = Game { _board :: Board
                 , _players :: [Player]
                 , _currentPlayer :: Player
                 , _history :: [Board]
                 } deriving (Eq)

makeLenses ''Game
