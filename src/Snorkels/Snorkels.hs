{-# LANGUAGE FlexibleInstances #-}

module Snorkels.Snorkels ( display ) where

import Control.Lens
import Data.Char
import Data.List
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
        (Just Stone) -> "O"
        Nothing -> " "

instance Displayable Board where
    display b = intercalate "\n"
                [concat
                    [concat ["[", display (B.getPiece b (x, y)), "]"]
                        | x <- [0..width-1]]
                        | y <- [0..height-1]
                ]
                where (width, height) = (b^.size)
