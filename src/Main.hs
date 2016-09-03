module Main where

import Snorkels.UI


main :: IO ()
main = do game <- create GameOptions {numStones = 3, boardSize = (7, 7), numPlayers = 2}
          play game
          return ()
