module Snorkels.Play ( play ) where


import Control.Monad.Loops (iterateUntilM)
import Data.Function
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List (partition)

import Snorkels.Types
import qualified Snorkels.Game as G


getCurrentPlayerType :: Game -> PlayerType
getCurrentPlayerType game = (game&playerTypes) Map.! (game&currentPlayer)


playMove :: Game -> Maybe String -> IO Game
playMove game errorMessage = do move <- getMove pt game errorMessage
                                case move of
                                  Nothing -> return $ G.quit game
                                  Just pos -> case G.move pos game of
                                                Left message -> playMove game $ Just message
                                                Right game -> return game
                             where pt = getCurrentPlayerType game


playSwitch :: Game -> Maybe String -> IO Game
playSwitch game errorMessage = do pos <- getSwitch pt game errorMessage
                                  case G.switch pos game of
                                    Left message -> playSwitch game $ Just message
                                    Right game -> return game
                               where pt = getCurrentPlayerType game


playRound :: (Game -> Maybe String -> IO Game) -> Game -> IO Game
playRound playFunc game = do g <- playFunc game Nothing
                             if G.hasFinished g || ((game&currentPlayer) > (g&currentPlayer))
                             then return g
                             else playRound playFunc g


reportWinnerAround :: Game -> IO ()
reportWinnerAround game = case G.getWinner game of
                            Just winner -> do mapM_ (reportToPT winner) nonlocals
                                              maybe (return ()) (reportToPT winner) (listToMaybe locals)
                            Nothing -> return ()
                           where pts = Map.elems $ game&playerTypes
                                 (locals, nonlocals) = partition isLocal pts
                                 reportToPT winner pt = reportWinner pt game winner



play :: Game -> IO Game
play game = do g <- playRound playMove game
               -- The first moving round is over, ask for switches
               g <- G.makeSwitches <$> playRound playSwitch g
               -- Continue until finished
               g <- iterateUntilM G.hasFinished (playRound playMove) g
               -- The game has now finished
               reportWinnerAround g
               return g
