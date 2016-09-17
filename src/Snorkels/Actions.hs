module Snorkels.Actions ( getMove
                        , getSwitch
                        , reportWinner
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


type Action = Either Player (Maybe Position)


doAction :: Action -> Game -> Game
doAction (Left player) = switch player
doAction (Right Nothing) = quit
doAction (Right (Just pos)) = move pos


getMove' :: PlayerType -> Game -> Maybe String -> IO (Maybe Position)
getMove' (LocalPlayer config) = localMove config
getMove' (ComputerPlayer config) = randomMove config


getMove :: Game -> Maybe String -> IO Action
getMove game err = do let pt = getCurrentPlayerType game
                      action <- getMove' pt game err
                      case action of
                        Nothing -> return $ Right Nothing  -- Player has quit
                        Just pos -> if isValidMove pos game
                                       then return $ Right $ Just pos -- Player has correctly moved
                                       else getMove game (Just "Cannot place a snorkel there.")


getSwitch' :: PlayerType -> Game -> Maybe String -> IO Player
getSwitch' (LocalPlayer config) = localSwitch config
getSwitch' (ComputerPlayer config) = randomSwitch config


getSwitch :: Game -> Maybe String -> IO Action
getSwitch game err = do let pt = getCurrentPlayerType game
                        player <- getSwitch' pt game err
                        if isValidSwitch player game
                           then return $ Left player
                           else getSwitch game (Just "Cannot switch to such color.")


reportWinner :: PlayerType -> Game -> Player -> IO ()
reportWinner (LocalPlayer config) = localReportWinner config
reportWinner (ComputerPlayer config) = randomReportWinner config


playRound :: (Game -> Maybe String -> IO Action) -> Game -> IO Game
playRound playFunc g = do action <- playFunc g Nothing
                          let t = doAction action g
                          if hasFinished t || ((g&currentPlayer) > (t&currentPlayer))
                          then return t
                          else playRound playFunc t


reportWinnerAround :: Game -> IO ()
reportWinnerAround game = case getWinner game of
                            Just winner -> mapM_ (reportToPT winner) pts
                            Nothing -> return ()
                           where pts = Map.elems $ game&playerTypes
                                 reportToPT winner pt = reportWinner pt game winner



play :: Game -> IO Game
play g = do g <- playRound getMove g
            -- The first moving round is over, ask for switches
            g <- makeSwitches <$> playRound getSwitch g
            -- Continue until finished
            g <- iterateUntilM hasFinished (playRound getMove) g
            -- The game has now finished
            reportWinnerAround g
            return g
