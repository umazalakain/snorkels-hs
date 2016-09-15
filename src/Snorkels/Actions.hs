module Snorkels.Actions ( getMove
                        , getSwitch
                        , reportWinner
                        , makeSwitches
                        , quit
                        , move
                        , switch
                        ) where

import qualified Data.Bimap as Bimap
import Data.Function
import qualified Data.Map.Strict as Map
import Data.Maybe

import Snorkels.Board
import Snorkels.Game
import Snorkels.PlayerTypes.CLI (cliMove, cliSwitch, cliReportWinner)
import Snorkels.PlayerTypes.RandomAgent (randomMove, randomSwitch, randomReportWinner)


getMove :: PlayerType -> Game -> Maybe String -> IO (Maybe Position)
getMove (LocalPlayer config) = cliMove config
getMove (ComputerPlayer config) = randomMove config


getSwitch :: PlayerType -> Game -> Maybe String -> IO Player
getSwitch (LocalPlayer config) = cliSwitch config
getSwitch (ComputerPlayer config) = randomSwitch config


reportWinner :: PlayerType -> Game -> Player -> IO ()
reportWinner (LocalPlayer config) = cliReportWinner config
reportWinner (ComputerPlayer config) = randomReportWinner config


quit :: Game -> Game
quit game = (advancePlayer game) {playerTypes = Map.delete (game&currentPlayer) (game&playerTypes)}


move :: Position -> Game -> Either String Game
move pos game
    | not validPosition = Left "Cannot place a snorkel there."
    | not survivors = Left "No surviving players left."
    | otherwise = Right $ advancePlayer . putSnorkel $ game
    where validPosition = elem pos $ freePositions $ game&board
          survivors = isJust $ getNextPlayer game
          putSnorkel g = g {board = putPiece (g&board) pos $ Snorkel (g&currentPlayer)}


switch :: Player -> Game -> Either String Game
switch player game
    | player `notElem` validSwitches game = Left "Cannot switch to such color."
    | otherwise = Right $ nextPlayer . putSwitch $ game
    where putSwitch g = g {switches = Bimap.insert (g&currentPlayer) player (g&switches)}
          nextPlayer g = g {currentPlayer = fromJust (getNextPlayer g)}


makeSwitches :: Game -> Game
makeSwitches game = game {playerTypes = Map.mapKeys getChosen $ game&playerTypes}
                    where getChosen p = fromMaybe p (Bimap.lookup p $ game&switches)


