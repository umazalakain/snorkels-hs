module Snorkels.PlayerTypes.Network ( server ) where

import Network
import Control.Concurrent
import System.IO

import Snorkels.Game


-- BUFFER: [(Player, Either (Maybe Position) Switch, Board)]
-- TODO: Switches should be checked too

-- Each program instance maintains a history buffer
-- When it's a network player's turn, connect to its server and asks for change
-- length localBuffer + 1

-- We could generalize this buffer thing so that all player types receive it

server :: MVar Game -> IO a
server mVarGame = do sock <- listenOn $ PortNumber 7777
                     loop sock mVarGame


loop :: Socket -> MVar Game -> IO a
loop sock mVarGame = do (h,_,_) <- accept sock
                        forkIO $ body h mVarGame
                        loop sock mVarGame
                     where
                         body h mVarGame = do
                             game <- takeMVar mVarGame
                             hPutStr h $ show $ currentPlayer game
                             hFlush h
                             hClose h
