module Main where

import Snorkels.Play
import Snorkels.UI


main :: IO ()
main = do game <- create GameOptions {optNumStones = 3, optBoardSize = (7, 7), optNumPlayers = 3}
          play game
          return ()
