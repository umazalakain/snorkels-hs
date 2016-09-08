module Snorkels.Game ( move
                     , switch
                     , quit
                     , getSurvivors
                     , getNextPlayer
                     , getWinner
                     , hasFinished
                     , validSwitches
                     , makeSwitches
                     ) where

import Control.Lens
import Data.Maybe
import Data.List
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Snorkels.Types
import qualified Snorkels.Board as B


getSurvivors :: Game -> [Player]
getSurvivors game = case filter hasSurvived players of
                         [] -> [game^.currentPlayer]
                         s -> s
                    where hasSurvived = not . B.hasLost game
                          players = game^.playerTypes.to Map.keys


getNextPlayer :: Game -> Maybe Player
getNextPlayer game = listToMaybe
                   . drop 1
                   . dropWhile (/= game^.currentPlayer)
                   $ cycle
                   $ getSurvivors game


quit :: Game -> Game
-- TODO
quit game = game


move :: Position -> Game -> Either String Game
move pos game
    | not validPosition = Left "Cannot place a snorkel there."
    | not survivors = Left "No surviving players left."
    | otherwise = Right $ nextPlayer . putSnorkel $ game
    where validPosition = elem pos $ B.freePositions game
          survivors = isJust $ getNextPlayer game
          putSnorkel g = B.putPiece g pos $ Snorkel (game^.currentPlayer)
          nextPlayer g = g & currentPlayer .~ fromJust (getNextPlayer g)


validSwitches :: Game -> [Player]
validSwitches game = getSurvivors game \\ Bimap.keysR (game^.switches)


switch :: Player -> Game -> Either String Game
switch player game
    | player `notElem` validSwitches game = Left "Cannot switch to such color."
    | otherwise = Right $ nextPlayer . putSwitch $ game
    where putSwitch g = g & switches .~ Bimap.insert (g^.currentPlayer) player (g^.switches)
          nextPlayer g = g & currentPlayer .~ fromJust (getNextPlayer g)


makeSwitches :: Game -> Game
makeSwitches game = game & playerTypes %~ Map.mapKeys getChosen
                    where getChosen p = fromMaybe p (Bimap.lookup p $ game^.switches)


getWinner :: Game -> Maybe Player
getWinner game = case getSurvivors game of
                      [x] -> Just x
                      _ -> Nothing


hasFinished :: Game -> Bool
hasFinished = isJust . getWinner
