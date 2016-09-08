{-# LANGUAGE FlexibleInstances #-}

module Snorkels.CLI ( GameOptions (..)
                    , create
                    ) where

import Control.Lens
import Data.Char
import Data.List
import System.Random (getStdGen)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Parsec (parse, (<|>))
import Text.Parsec.Char (string, spaces)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (choice)
import Text.ParserCombinators.Parsec.Number (nat)
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Console.ANSI as ANSI

import Snorkels.Types
import qualified Snorkels.Board as B
import qualified Snorkels.Game as G


class Displayable a where
    display :: a -> String


snorkelColour c = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]
reset = ANSI.setSGRCode [ANSI.Reset]

instance Displayable (Maybe Piece) where
    display s = case s of
        (Just (Snorkel Green)) -> concat [snorkelColour ANSI.Green, "G", reset]
        (Just (Snorkel Purple)) -> concat [snorkelColour ANSI.Magenta, "P", reset]
        (Just (Snorkel Red)) -> concat [snorkelColour ANSI.Red, "R", reset]
        (Just (Snorkel Yellow)) -> concat [snorkelColour ANSI.Yellow, "Y", reset]
        (Just (Snorkel Cyan)) -> concat [snorkelColour ANSI.Cyan, "C", reset]
        (Just Stone) -> "O"
        Nothing -> " "

instance Displayable Game where
    display g = intercalate "\n"
                [concat
                    [concat ["[", display (B.getPiece g (x, y)), "]"]
                        | x <- [0..width-1]]
                        | y <- [0..height-1]
                ]
                where (width, height) = g^.boardSize


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
                                      , _playerTypes = Map.fromList [(p, cli) | p <- players]
                                      , _currentPlayer = Green
                                      , _switches = Bimap.empty
                                      }


playerRepr :: Bimap.Bimap Player String
playerRepr = Bimap.fromList [(p, show p) | p <- [Green ..]]


moveParser :: Parser (Maybe Position)
moveParser = do spaces
                x <- nat
                spaces
                y <- nat
                spaces
                -- TODO: QUIT
                return $ Just (x, y)


switchParser :: Parser Player
switchParser = do spaces
                  -- TODO: Limit choices to unchosen players
                  player <- choice $ map string $ Bimap.keysR playerRepr
                  spaces
                  return $ playerRepr Bimap.!> player 


readParser :: Parser a -> Game -> IO a
readParser parser game = do putStr $ printf "%s: " $ show $ game^.currentPlayer
                            hFlush stdout
                            input <- getLine
                            case parse parser "" input of
                              Left parseError -> do print parseError
                                                    readParser parser game
                              Right result -> return result


cliMove :: Game -> IO (Maybe Position)
cliMove game = do putStrLn $ display game
                  readParser moveParser game


cliSwitch :: Game -> IO Player
cliSwitch game = do putStrLn "Choose the player you want to switch to"
                    readParser switchParser game


cli :: PlayerType
cli = PlayerType { getMove = cliMove
                 , getSwitch = cliSwitch
                 , reportError = print
                 }
