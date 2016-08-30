module Main where

import qualified Data.Map.Strict as Map

import Snorkels.UI
import Snorkels.Types
import qualified Snorkels.Board as B
import qualified Snorkels.Game as G




sampleBoard :: Board
sampleBoard = Board { _pieces = (Map.fromList [((0, 0), Snorkel Green),
                                               ((0, 1), Snorkel Green),
                                               ((0, 3), Snorkel Green),
                                               ((0, 2), Snorkel Purple),
                                               ((1, 2), Snorkel Green),
                                               ((2, 4), Stone)])
                    , _size = (10, 10)
                    }


sampleGame :: Game
sampleGame = Game { _board = sampleBoard
                  , _players = [Green, Purple]
                  , _currentPlayer = Green
                  , _history = []
                  }

main :: IO ()
main = do play sampleGame
          return ()
