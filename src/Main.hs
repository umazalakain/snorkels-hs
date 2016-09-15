module Main where

import qualified Data.Bimap as Bimap
import Data.Function
import qualified Data.Map.Strict as Map
import Data.Monoid
import System.Random (getStdGen)

import Options.Applicative

import Snorkels.Play
import Snorkels.CLI (cli)
import Snorkels.RandomAgent (randomAgent)
import Snorkels.Board
import Snorkels.Game


data MainParser = MainParser { optPlayers :: [PlayerConfig]
                             , optNumStones :: Int
                             , optWidth :: Int
                             , optHeight :: Int
                             } deriving (Eq)


data PlayerConfig = Local | Computer
    deriving (Eq, Show)


localParser :: Parser PlayerConfig
localParser = flag' Local (short 'l')


computerParser :: Parser PlayerConfig
computerParser = flag' Computer (short 'c')


mainParser :: Parser MainParser
mainParser = MainParser
        <$> many (localParser <|> computerParser)
        <*> option auto
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


getPlayerType :: PlayerConfig -> PlayerType
getPlayerType Local = cli
getPlayerType Computer = randomAgent


create :: MainParser -> IO (Either String Game)
create options
    | max (options&optWidth) (options&optHeight) > 26 = return $ Left "Cannot have more than 26 on either axis."
    | length (options&optPlayers) < 2 = return $ Left "Snorkels must be played by at least 2 players."
    | otherwise = do g <- getStdGen
                     case throwStones (game&board) (options&optNumStones) g of
                       Left message -> return $ Left message
                       Right board -> return $ Right $ game { board = board}
    where players = Map.fromList [(p, getPlayerType c) | (p, c) <- zip [Green ..] (options&optPlayers)]
          game = Game { board = Board { pieces = Map.empty
                                      , size = (optWidth options, optHeight options)
                                      }
                      , playerTypes = players
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

