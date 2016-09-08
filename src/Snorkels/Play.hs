module Snorkels.Play ( GameOptions (..)
                     , create
                     , play
                     ) where


import Control.Lens
import Control.Monad.Loops (iterateUntilM)
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
                          game = Game { _pieces = Map.empty
                                      , _boardSize = optBoardSize options
                                      , _playerTypes = Map.fromList [(p, CLI.cli) | p <- players]
                                      , _currentPlayer = Green
                                      , _switches = Bimap.empty
                                      }


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
