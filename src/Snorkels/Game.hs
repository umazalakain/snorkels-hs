module Snorkels.Game ( Action (..)
                     , validateAction
                     , doAction
                     , move
                     , getSurvivors
                     , getWinner
                     ) where

import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Snorkels.Types
import qualified Snorkels.Board as B


getSurvivors :: Game -> [Player]
getSurvivors game = case filter hasSurvived $ game^.players of
                         [] -> [game^.currentPlayer]
                         s -> s
                    where hasSurvived = (not . (B.hasLost game))


nextPlayer :: Game -> Game
nextPlayer game = game & currentPlayer .~ nextPlayer
                   where survivors = getSurvivors game
                         nextPlayer = head
                                    . drop 1
                                    . dropWhile (/= game^.currentPlayer)
                                    $ cycle survivors


move :: Position -> Game -> Game
move pos game = nextPlayer $ B.putPiece game pos piece
                where piece = Snorkel (game^.currentPlayer)


getWinner :: Game -> Maybe Player
getWinner game = case getSurvivors game of
                      (x:[]) -> Just x
                      _ -> Nothing




data Action = Move Position | Switch Player | Quit


-- Maybe [Char] should be some type of exception
validateAction :: Action -> Game -> Maybe [Char]
validateAction (Move pos) game
        | elem pos $ B.freePositions game = Nothing
        | otherwise = Just "Cannot place a snorkel there."
-- TODO: Define for switch
-- TODO: Define for quit


doAction :: Action -> Game -> Game
doAction (Move pos) game = move pos game
-- TODO: Define for switch
-- TODO: Define for quit


{-getNumberOfMoves :: Game -> Int-}
{-getNumberOfMoves -}
