module Snorkels.Play ( play ) where


import Control.Monad.Loops (iterateUntilM)
import Data.Function
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List (partition)

import Snorkels.Game


getCurrentPlayerType :: Game -> PlayerType
getCurrentPlayerType game = (game&playerTypes) Map.! (game&currentPlayer)


playMove :: Game -> Maybe String -> IO Game
playMove game errorMessage = do m <- getMove pt game errorMessage
                                case m of
                                  Nothing -> return $ quit game
                                  Just pos -> case move pos game of
                                                Left message -> playMove game $ Just message
                                                Right game -> return game
                             where pt = getCurrentPlayerType game


playSwitch :: Game -> Maybe String -> IO Game
playSwitch game errorMessage = do pos <- getSwitch pt game errorMessage
                                  case switch pos game of
                                    Left message -> playSwitch game $ Just message
                                    Right game -> return game
                               where pt = getCurrentPlayerType game


playRound :: (Game -> Maybe String -> IO Game) -> Game -> IO Game
playRound playFunc game = do g <- playFunc game Nothing
                             if hasFinished g || ((game&currentPlayer) > (g&currentPlayer))
                             then return g
                             else playRound playFunc g


reportWinnerAround :: Game -> IO ()
reportWinnerAround game = case getWinner game of
                            Just winner -> do mapM_ (reportToPT winner) nonlocals
                                              maybe (return ()) (reportToPT winner) (listToMaybe locals)
                            Nothing -> return ()
                           where pts = Map.elems $ game&playerTypes
                                 (locals, nonlocals) = partition isLocal pts
                                 reportToPT winner pt = reportWinner pt game winner



play :: Game -> IO Game
play game = do g <- playRound playMove game
               -- The first moving round is over, ask for switches
               g <- makeSwitches <$> playRound playSwitch g
               -- Continue until finished
               g <- iterateUntilM hasFinished (playRound playMove) g
               -- The game has now finished
               reportWinnerAround g
               return g
