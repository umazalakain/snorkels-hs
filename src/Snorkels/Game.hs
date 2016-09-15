module Snorkels.Game ( LocalConfig (..)
                     , ComputerConfig (..)
                     , PlayerType (..)
                     , Game (..)
                     , isLocal
                     , getSurvivors
                     , advancePlayer
                     , getNextPlayer
                     , getWinner
                     , hasFinished
                     , validSwitches
                     ) where

import Data.Function
import Data.Maybe
import Data.List
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map

import Snorkels.Board


data LocalConfig = LocalConfig
                   deriving (Eq)
data ComputerConfig = Random | Easy | Medium | Hard
                      deriving (Eq)
data PlayerType = LocalPlayer LocalConfig | ComputerPlayer ComputerConfig
                  deriving (Eq)


data Game = Game {
      board :: Board
    -- | List of 'Player's that didn't 'Quit'
    , playerTypes :: Map.Map Player PlayerType
    -- | 'Player' that plays next
    , currentPlayer :: Player
    -- | Map of what each player chooses to be when asked to switch
    , switches :: Bimap.Bimap Player Player
}


isLocal :: PlayerType -> Bool
isLocal (LocalPlayer _) = True
isLocal _ = False


getSurvivors :: Game -> [Player]
getSurvivors game = case filter hasSurvived players of
                         [] -> [game&currentPlayer]
                         s -> s
                    where hasSurvived = not . hasLost (game&board)
                          players = Map.keys $ game&playerTypes


getNextPlayer :: Game -> Maybe Player
getNextPlayer game = listToMaybe
                   . drop 1
                   . dropWhile (/= (game&currentPlayer))
                   $ cycle
                   $ getSurvivors game


advancePlayer :: Game -> Game
advancePlayer game = game {currentPlayer = fromJust (getNextPlayer game)}


validSwitches :: Game -> [Player]
validSwitches game = getSurvivors game \\ Bimap.keysR (game&switches)


getWinner :: Game -> Maybe Player
getWinner game = case getSurvivors game of
                      [x] -> Just x
                      _ -> Nothing


hasFinished :: Game -> Bool
hasFinished = isJust . getWinner
