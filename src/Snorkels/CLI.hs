{-# LANGUAGE FlexibleInstances #-}

module Snorkels.CLI ( cli ) where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Parsec (parse, (<|>))
import Text.Parsec.Char (string, spaces)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (choice)
import Text.ParserCombinators.Parsec.Number (nat)
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
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
                where (width, height) = g&boardSize


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
readParser parser game = do putStr $ printf "%s: " $ show $ game&currentPlayer
                            hFlush stdout
                            input <- getLine
                            case parse parser "" input of
                              Left parseError -> do print parseError
                                                    readParser parser game
                              Right result -> return result


cliMove :: Game -> Maybe String -> IO (Maybe Position)
cliMove game errorMessage = do putStrLn $ display game
                               mapM_ putStrLn errorMessage
                               readParser moveParser game


cliSwitch :: Game -> Maybe String -> IO Player
cliSwitch game errorMessage = do putStrLn $ display game
                                 mapM_ putStrLn errorMessage
                                 putStr "Pick the color you want to switch to: "
                                 print $ G.validSwitches game
                                 readParser switchParser game


cliReportWinner :: Game -> Player -> IO ()
cliReportWinner _ player = putStrLn $ printf "%s has won!" $ show player

cli :: PlayerType
cli = PlayerType { getMove = cliMove
                 , getSwitch = cliSwitch
                 , reportWinner = cliReportWinner
                 }
