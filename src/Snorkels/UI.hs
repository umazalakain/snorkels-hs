{-# LANGUAGE FlexibleInstances #-}

module Snorkels.UI ( GameOptions (..)
                   , create
                   , play 
                   , playTurn
                   ) where

import Control.Monad.Loops
import Control.Lens
import Data.Char
import Data.List
import Data.Maybe
import System.Random
import System.IO
import Text.Printf
import Text.Regex.PCRE
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
                where (width, height) = (g^.boardSize)


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
                                      }




-- TODO: Parse other actions too
parseAction :: [Char] -> Maybe G.Action
parseAction input = let (_, _, _, pos) = input =~ "\\s*(\\d+)\\s+(\\d+)\\s*" :: (String, String, String, [String]) in
                    -- TODO: This is horrible, and it doesn't check bounds
                    Just $ G.Move (read $ pos !! 0 :: Int, read $ pos !! 1 :: Int)


-- TODO: Print error message on erroneous input
readAction :: Game -> IO (Maybe G.Action)
readAction game = do putStr $ printf "%s: " $ show $ game^.currentPlayer
                     hFlush stdout
                     input <- getLine
                     return $ parseAction input


playTurn :: Game -> IO Game
playTurn game = do putStrLn $ display game
                   action <- untilJust $ readAction game
                   case G.doAction action game of
                        Left message -> do putStrLn message
                                           return game
                        Right game -> return game


play :: Game -> IO Game
play game = do g <- iterateUntilM (isJust . G.getWinner) playTurn game
               putStrLn $ display g
               -- TODO: print winner
               return g
