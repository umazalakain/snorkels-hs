module Snorkels.Game ( LocalConfig (..)
                     , ComputerConfig (..)
                     , PlayerType (..)
                     , Game (..)
                     , getSurvivors
                     , getNextPlayer
                     , advancePlayer
                     , getCurrentPlayerType
                     , getWinner
                     , hasFinished
                     , validSwitches
                     , makeSwitches
                     , quit
                     , isValidMove
                     , move
                     , isValidSwitch
                     , switch
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


getCurrentPlayerType :: Game -> PlayerType
getCurrentPlayerType game = (game&playerTypes) Map.! (game&currentPlayer)


getWinner :: Game -> Maybe Player
getWinner game = case getSurvivors game of
                      [x] -> Just x
                      _ -> Nothing


hasFinished :: Game -> Bool
hasFinished = isJust . getWinner


quit :: Game -> Game
quit game = (advancePlayer game) {playerTypes = Map.delete (game&currentPlayer) (game&playerTypes)}


isValidMove :: Position -> Game -> Bool
isValidMove pos game = elem pos $ freePositions $ game&board


move :: Position -> Game -> Game
move pos = advancePlayer . putSnorkel pos
    where putSnorkel pos g = g {board = putPiece (g&board) pos $ Snorkel (g&currentPlayer)}


validSwitches :: Game -> [Player]
validSwitches game = getSurvivors game \\ Bimap.keysR (game&switches)


isValidSwitch :: Player -> Game -> Bool
isValidSwitch player game = player `elem` validSwitches game


switch :: Player -> Game -> Game
switch player = advancePlayer . putSwitch
    where putSwitch g = g {switches = Bimap.insert (g&currentPlayer) player (g&switches)}


makeSwitches :: Game -> Game
makeSwitches game = game {playerTypes = Map.mapKeys getChosen $ game&playerTypes}
                    where getChosen p = fromMaybe p (Bimap.lookup p $ game&switches)


