module Snorkels.PlayerTypes.RandomAgent ( randomMove
                                        , randomSwitch
                                        , randomReportWinner
                                        ) where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import System.Random (getStdGen)

import Snorkels.Board
import Snorkels.Game


randomMove :: ComputerConfig -> Game -> Maybe String -> IO (Maybe Position)
randomMove _ game errorMessage = do g <- getStdGen
                                    case shufflePositions (freePositions $ game&board) g of
                                      [] -> return Nothing
                                      (x:xs) -> return $ Just x


randomSwitch :: ComputerConfig -> Game -> Maybe String -> IO Player
randomSwitch _ game errorMessage = return $ head $ validSwitches game


randomReportWinner :: ComputerConfig -> Game -> Player -> IO ()
randomReportWinner _ game player = return ()
