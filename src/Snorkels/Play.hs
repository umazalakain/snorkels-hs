module Snorkels.Play ( GameOptions (..)
                     , create
                     , play
                     ) where


import Control.Monad.Loops (iterateUntilM)
import Data.Function
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import System.Random (getStdGen)

import Snorkels.Types
import qualified Snorkels.CLI as CLI
import qualified Snorkels.Game as G
import qualified Snorkels.Board as B


data GameOptions = GameOptions { optNumStones :: Int
                               , optBoardSize :: (Int, Int)
                               , optNumPlayers :: Int
                               } deriving (Eq)


create :: GameOptions -> IO Game
create options = do g <- getStdGen
                    return $ B.throwStones game (optNumStones options) g
                    where players = take (optNumPlayers options) [Green ..]
                          game = Game { pieces = Map.empty
                                      , boardSize = optBoardSize options
                                      , playerTypes = Map.fromList [(p, CLI.cli) | p <- players]
                                      , currentPlayer = Green
                                      , switches = Bimap.empty
                                      }


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


play :: Game -> IO Game
play game = do g <- playRound playMove game
               g <- G.makeSwitches <$> playRound playSwitch g
               -- And continue until finished
               g <- iterateUntilM G.hasFinished (playRound playMove) g
               -- The game has now finished
               -- TODO: print winner
               return g
