module Main where

import qualified Network.WebSockets as WS
import GamePlay (startGame)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Server 

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state