{-# LANGUAGE FlexibleInstances #-}

module Snorkels.UI ( GameOptions (..)
                   , create
                   , play 
                   , playTurn
                   ) where

import Control.Monad.Loops (iterateUntilM)
import Control.Lens
import Data.Char
import Data.List
import System.Random (getStdGen)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Parsec (parse)
import Text.Parsec.Char (string, spaces)
import Text.Parsec.String (Parser)
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
                    where game = Game { _pieces = Map.empty
                                      , _boardSize = optBoardSize options
                                      , _players = take (optNumPlayers options) [Green ..]
                                      , _currentPlayer = Green
                                      , _switches = Bimap.empty
                                      }


actionParser :: Parser G.Action
actionParser = do string "move"
                  spaces
                  x <- nat
                  spaces
                  y <- nat
                  spaces
                  return $ G.Move (x, y)


readAction :: Game -> IO G.Action
readAction game = do putStr $ printf "%s: " $ show $ game^.currentPlayer
                     hFlush stdout
                     input <- getLine
                     case parse actionParser "" input of
                       Left parseError -> do print parseError
                                             readAction game
                       Right action -> return action


-- | Prompt the player for an action until the action is valid, then do it
playTurn :: Game -> IO Game
playTurn game = do putStrLn $ display game
                   action <- readAction game
                   case G.doAction action game of
                        Left message -> do print message
                                           playTurn game
                        Right game -> return game


-- |
-- A cycle ends when the next player comes before the current one in the
-- player's list
playCycle :: Game -> IO Game
playCycle game = do g <- playTurn game
                    if G.hasFinished g || (game^.currentPlayer > g^.currentPlayer)
                    then return g
                    else playCycle g


play :: Game -> IO Game
play game = do g <- playCycle game
               -- We can now ask for color switches
               g <- iterateUntilM G.hasFinished playCycle g
               -- The game has now finished
               putStrLn $ display g
               -- TODO: print winner
               return g
