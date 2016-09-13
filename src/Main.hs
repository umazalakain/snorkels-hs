module Main where

import qualified Data.Bimap as Bimap
import Data.Function
import qualified Data.Map.Strict as Map
import Data.Monoid
import System.Random (getStdGen)

import Options.Applicative

import Snorkels.Types
import Snorkels.Play
import Snorkels.CLI (cli)
import Snorkels.RandomAgent (randomAgent)
import qualified Snorkels.Board as B


data MainParser = MainParser { optNumStones :: Int
                             , optWidth :: Int
                             , optHeight :: Int
                             , optNumPlayers :: Int
                             } deriving (Eq)


mainParser :: Parser MainParser
mainParser = MainParser
        <$> option auto
            ( long "stones"
           <> short 's'
           <> value 3
           <> metavar "#STONES"
           <> help "Number of stones randomly thrown on the board"
            )
        <*> option auto
            ( long "width"
           <> short 'w'
           <> value 7
           <> metavar "WIDTH"
           <> help "Width of the board"
            )
        <*> option auto
            ( long "height"
           <> short 'h'
           <> value 7
           <> metavar "HEIGHT"
           <> help "Height of the board"
            )
        <*> option auto
            ( long "players"
           <> short 'p'
           <> value 2
           <> metavar "#PLAYERS"
           <> help "Number of players"
            )


create :: MainParser -> IO (Either String Game)
create options
    | max (options&optWidth) (options&optHeight) > 26 = return $ Left "Cannot have more than 26 on either axis."
    | otherwise = do g <- getStdGen
                     return $ B.throwStones game (optNumStones options) g
    where players = take (optNumPlayers options) [Green ..]
          game = Game { pieces = Map.empty
                      , boardSize = (optWidth options, optHeight options)
                      , playerTypes = Map.fromList [(Green, cli), (Purple, randomAgent)]
                      , currentPlayer = Green
                      , switches = Bimap.empty
                      }


run :: MainParser -> IO ()
run options = do game <- create options
                 case game of
                   Left message -> print message
                   Right g -> do play g
                                 return ()


main :: IO ()
main = execParser opts >>= run
       where
           opts = info (helper <*> mainParser)
             ( fullDesc
            <> progDesc "TODO"
            <> header "Snorkels -- strategic board game")

