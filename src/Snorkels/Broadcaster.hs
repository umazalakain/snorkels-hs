{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Snorkels.Broadcaster ( Snapshot
                            , createChannel
                            , broadcast
                            , test
                            ) where


import Network
import Control.Concurrent
import Control.Concurrent.Chan
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


createChannel :: IO (Chan Snapshot)
createChannel = newChan


broadcast :: Chan Snapshot -> IO ThreadId
broadcast channel = forkIO $ do sock <- listenOn $ PortNumber 7777
                                -- Make a duplicate of the channel for this
                                -- client
                                channel <- dupChan channel
                                serve sock channel
                                return ()


serve :: Socket -> Chan Snapshot -> IO ThreadId
serve sock channel = do (h,_,_) <- accept sock
                        forkIO $ body h channel
                     where
                         body h channel = do
                             snaps <- getChanContents channel
                             hPut h $ encode snaps
                             hFlush h
                             hClose h


test = do chan <- createChannel
          let s = (Green, Left Nothing, Board (Map.fromList []) (10, 10), Bimap.fromList [])
          writeChan chan s
          broadcast chan

