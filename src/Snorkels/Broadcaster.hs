{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Snorkels.Broadcaster ( TChan
                            , Action
                            , Snapshot
                            , createChannel
                            , sendSnapshot
                            , broadcast
                            ) where


import Network
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy
import Data.Function
import System.IO

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map

import Snorkels.Board
import Snorkels.Game


type Action = Either Player (Maybe Position)
type Snapshot = (Player, Action, Board, Bimap.Bimap Player Player)
$(deriveJSON defaultOptions ''Snorkel)
$(deriveJSON defaultOptions ''Board)
$(deriveJSON defaultOptions ''Piece)
$(deriveJSON defaultOptions ''Map.Map)
$(deriveJSON defaultOptions ''Bimap.Bimap)


createChannel :: IO (TChan Snapshot)
createChannel = newTChanIO


sendSnapshot :: Player -> Action -> Game -> TChan Snapshot -> IO ()
sendSnapshot player action game chan = atomically $ writeTChan chan snap
    where snap = (player, action, game&board, game&switches)


broadcast :: TChan Snapshot -> IO ThreadId
broadcast broadcastChan = forkIO $ do sock <- listenOn $ PortNumber 7777
                                      forever $ do
                                          (h,_,_) <- accept sock
                                          forkIO $ respond h broadcastChan


respond :: Handle -> TChan Snapshot -> IO ()
respond handle broadcastChan = do clientChan <- atomically $ cloneTChan broadcastChan
                                  dump handle clientChan


dump :: Handle -> TChan Snapshot -> IO ()
dump handle clientChan = do read <- atomically $ tryReadTChan clientChan
                            case read of
                              Nothing -> hClose handle
                              Just snapshot -> do hPut handle $ encode snapshot
                                                  hFlush handle
                                                  dump handle clientChan
