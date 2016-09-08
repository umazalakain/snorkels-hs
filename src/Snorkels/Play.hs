module Snorkels.Play (
                       play
                     ) where


import Control.Lens
import Control.Monad.Loops (iterateUntilM)
import qualified Data.Map.Strict as Map

import Snorkels.Types
import qualified Snorkels.Game as G


getCurrentPlayerType :: Game -> PlayerType
getCurrentPlayerType game = (game^.playerTypes) Map.! (game^.currentPlayer)


playMove :: Game -> IO Game
playMove game = do move <- getMove pt game
                   case move of
                     Nothing -> return $ G.quit game
                     Just pos -> case G.move pos game of
                                   Left message -> do reportError pt message
                                                      playMove game
                                   Right game -> return game
                where pt = getCurrentPlayerType game


playSwitch :: Game -> IO Game
playSwitch game = do pos <- getSwitch pt game
                     case G.switch pos game of
                       Left message -> do reportError pt message
                                          playSwitch game
                       Right game -> return game
                  where pt = getCurrentPlayerType game


playRound :: (Game -> IO Game) -> Game -> IO Game
playRound playFunc game = do g <- playFunc game
                             if G.hasFinished g || (game^.currentPlayer > g^.currentPlayer)
                             then return g
                             else playRound playFunc g


play :: Game -> IO Game
play game = do g <- playRound playMove game
               g <- G.makeSwitches <$> playRound playSwitch g
               -- And continue until finished
               g <- iterateUntilM G.hasFinished (playRound playMove) g
               -- The game has now finished
               -- TODO: print winner
               return g
