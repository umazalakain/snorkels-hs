{-# LANGUAGE FlexibleInstances #-}

module Snorkels.RandomAgent ( randomAgent ) where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import System.Random (getStdGen)

import Snorkels.Board
import Snorkels.Game


randomMove :: Game -> Maybe String -> IO (Maybe Position)
randomMove game errorMessage = do g <- getStdGen
                                  case shufflePositions (freePositions $ game&board) g of
                                    [] -> return Nothing
                                    (x:xs) -> return $ Just x


randomSwitch :: Game -> Maybe String -> IO Player
randomSwitch game errorMessage = return $ head $ validSwitches game


randomReportWinner :: Game -> Player -> IO ()
randomReportWinner game player = return ()

randomAgent :: PlayerType
randomAgent = PlayerType { getMove = randomMove
                         , getSwitch = randomSwitch
                         , reportWinner = randomReportWinner
                         , isLocal = False
                         }
