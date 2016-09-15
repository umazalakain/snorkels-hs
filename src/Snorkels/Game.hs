module Snorkels.Game ( PlayerType (..)
                     , Game (..)
                     , move
                     , switch
                     , quit
                     , getSurvivors
                     , getNextPlayer
                     , getWinner
                     , hasFinished
                     , validSwitches
                     , makeSwitches
                     ) where

import Data.Function
import Data.Maybe
import Data.List
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Snorkels.Board


data PlayerType = PlayerType {
    -- | Maybe print an error message and get a position to move to or a quit.
      getMove :: Game -> Maybe String -> IO (Maybe Position)
    -- | Maybe print an error message and get a player to switch to.
    , getSwitch :: Game -> Maybe String -> IO Player
    -- | Report the player about the winner of the game
    , reportWinner :: Game -> Player -> IO ()
    -- | Only one of the playertypes marked as local is notified about the
    -- winner. FIXME: There must be a cleverer way of doing this.
    , isLocal :: Bool
}


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


validSwitches :: Game -> [Player]
validSwitches game = getSurvivors game \\ Bimap.keysR (game&switches)


switch :: Player -> Game -> Either String Game
switch player game
    | player `notElem` validSwitches game = Left "Cannot switch to such color."
    | otherwise = Right $ nextPlayer . putSwitch $ game
    where putSwitch g = g {switches = Bimap.insert (g&currentPlayer) player (g&switches)}
          nextPlayer g = g {currentPlayer = fromJust (getNextPlayer g)}


makeSwitches :: Game -> Game
makeSwitches game = game {playerTypes = Map.mapKeys getChosen $ game&playerTypes}
                    where getChosen p = fromMaybe p (Bimap.lookup p $ game&switches)


getWinner :: Game -> Maybe Player
getWinner game = case getSurvivors game of
                      [x] -> Just x
                      _ -> Nothing


hasFinished :: Game -> Bool
hasFinished = isJust . getWinner
