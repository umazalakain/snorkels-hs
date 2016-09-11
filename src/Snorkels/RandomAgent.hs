{-# LANGUAGE FlexibleInstances #-}

module Snorkels.RandomAgent ( randomAgent ) where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import System.Random (getStdGen)

import Snorkels.Types
import qualified Snorkels.Board as B
import qualified Snorkels.Game as G


randomMove :: Game -> Maybe String -> IO (Maybe Position)
randomMove game errorMessage = do g <- getStdGen
                                  case B.shufflePositions (B.freePositions game) g of
                                    [] -> return Nothing
                                    (x:xs) -> return $ Just x


randomSwitch :: Game -> Maybe String -> IO Player
randomSwitch game errorMessage = return $ head $ G.validSwitches game


randomReportWinner :: Game -> Player -> IO ()
randomReportWinner game player = return ()

randomAgent :: PlayerType
randomAgent = PlayerType { getMove = randomMove
                         , getSwitch = randomSwitch
                         , reportWinner = randomReportWinner
                         , isLocal = False
                         }
