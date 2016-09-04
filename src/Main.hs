module Main where

import Snorkels.UI


main :: IO ()
main = do game <- create GameOptions {optNumStones = 3, optBoardSize = (7, 7), optNumPlayers = 2}
          play game
          return ()
