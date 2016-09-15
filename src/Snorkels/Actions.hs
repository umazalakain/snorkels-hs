module Snorkels.Actions ( getMove
                        , getSwitch
                        , reportWinner
                        , playMove
                        , playSwitch
                        , playRound
                        , reportWinnerAround
                        , play
                        ) where

import Control.Monad.Loops (iterateUntilM)
import qualified Data.Bimap as Bimap
import Data.Function
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List (partition)

import Snorkels.Board
import Snorkels.Game
import Snorkels.PlayerTypes.Local (localMove, localSwitch, localReportWinner)
import Snorkels.PlayerTypes.RandomAgent (randomMove, randomSwitch, randomReportWinner)


getMove :: PlayerType -> Game -> Maybe String -> IO (Maybe Position)
getMove (LocalPlayer config) = localMove config
getMove (ComputerPlayer config) = randomMove config


getSwitch :: PlayerType -> Game -> Maybe String -> IO Player
getSwitch (LocalPlayer config) = localSwitch config
getSwitch (ComputerPlayer config) = randomSwitch config


reportWinner :: PlayerType -> Game -> Player -> IO ()
reportWinner (LocalPlayer config) = localReportWinner config
reportWinner (ComputerPlayer config) = randomReportWinner config


playMove :: Game -> Maybe String -> IO Game
playMove game errorMessage = do let pt = getCurrentPlayerType game
                                m <- getMove pt game errorMessage
                                case m of
                                  Nothing -> return $ quit game
                                  Just pos -> case move pos game of
                                                Left message -> playMove game $ Just message
                                                Right game -> return game


playSwitch :: Game -> Maybe String -> IO Game
playSwitch game errorMessage = do let pt = getCurrentPlayerType game
                                  p <- getSwitch pt game errorMessage
                                  case switch p game of
                                    Left message -> playSwitch game $ Just message
                                    Right game -> return game


playRound :: (Game -> Maybe String -> IO Game) -> Game -> IO Game
playRound playFunc game = do g <- playFunc game Nothing
                             if hasFinished g || ((game&currentPlayer) > (g&currentPlayer))
                             then return g
                             else playRound playFunc g


reportWinnerAround :: Game -> IO ()
reportWinnerAround game = case getWinner game of
                            Just winner -> mapM_ (reportToPT winner) pts
                            Nothing -> return ()
                           where pts = Map.elems $ game&playerTypes
                                 reportToPT winner pt = reportWinner pt game winner



play :: Game -> IO Game
play g = do g <- playRound playMove g
            -- The first moving round is over, ask for switches
            g <- makeSwitches <$> playRound playSwitch g
            -- Continue until finished
            g <- iterateUntilM hasFinished (playRound playMove) g
            -- The game has now finished
            reportWinnerAround g
            return g
