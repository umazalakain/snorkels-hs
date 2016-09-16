{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Snorkels.Broadcaster ( Snapshot
                            , createChannel
                            , broadcast
                            , test
                            ) where


import Network
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy
import System.IO

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map

import Snorkels.Board
import Snorkels.Game


type Snapshot = (Player, Either (Maybe Position) Player, Board, Bimap.Bimap Player Player)
$(deriveJSON defaultOptions ''Snorkel)
$(deriveJSON defaultOptions ''Board)
$(deriveJSON defaultOptions ''Piece)
$(deriveJSON defaultOptions ''Map.Map)
$(deriveJSON defaultOptions ''Bimap.Bimap)


createChannel :: IO (TChan Snapshot)
createChannel = newTChanIO


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


test = do chan <- createChannel
          broadcast chan
          let s = (Green, Left Nothing, Board (Map.fromList []) (10, 10), Bimap.fromList [])
          let m = (Purple, Left Nothing, Board (Map.fromList []) (10, 10), Bimap.fromList [])
          let p = (Yellow, Left Nothing, Board (Map.fromList []) (10, 10), Bimap.fromList [])
          let q = (Red, Left Nothing, Board (Map.fromList []) (10, 10), Bimap.fromList [])
          atomically $ writeTChan chan s
          atomically $ writeTChan chan m
          atomically $ writeTChan chan p
          atomically $ writeTChan chan q
