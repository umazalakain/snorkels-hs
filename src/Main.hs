module Main where

import qualified Data.Bimap as Bimap
import Data.Function
import qualified Data.Map.Strict as Map
import System.Random (getStdGen)

import Options

import Snorkels.Types
import Snorkels.Play
import qualified Snorkels.CLI as CLI
import qualified Snorkels.Board as B


data MainOptions = MainOptions { optNumStones :: Int
                               , optWidth :: Int
                               , optHeight :: Int
                               , optNumPlayers :: Int
                               } deriving (Eq)


instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "num-stones" 3
            "Number of stones randomly thrown on the board"
        <*> simpleOption "width" 7
            "Width of the board"
        <*> simpleOption "height" 7
            "Height of the board"
        <*> simpleOption "players" 2
            "Number of players"


create :: MainOptions -> IO (Either String Game)
create options
    | max (options&optWidth) (options&optHeight) > 26 = return $ Left "Cannot have more than 26 on either axis."
    | otherwise = do g <- getStdGen
                     return $ B.throwStones game (optNumStones options) g
    where players = take (optNumPlayers options) [Green ..]
          game = Game { pieces = Map.empty
                      , boardSize = (optWidth options, optHeight options)
                      , playerTypes = Map.fromList [(p, CLI.cli) | p <- players]
                      , currentPlayer = Green
                      , switches = Bimap.empty
                      }


main :: IO ()
main = runCommand $ \opts args -> do
    game <- create opts
    case game of
      Left message -> print message
      Right g -> do play g
                    return ()
