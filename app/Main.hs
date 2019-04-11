{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import System.ZMQ4.Monadic
import Remote
import Data.Maybe

import qualified Data.ByteString.Char8 as BC

receiveAsync :: ZMQ z ()
receiveAsync = do
    asyncHandler <- socket Pull
    bind asyncHandler "tcp://*:45555"
    forever $ do
        buffer <- receive asyncHandler
        liftIO $ BC.putStrLn buffer
        result <- liftIO $ execRemoteCommand $ readCommand buffer
        case result of
            Nothing -> liftIO $ putStrLn "Async Handler: Error"
            _ -> liftIO $ putStrLn "Async Handler: Success"

receiveSync :: ZMQ z ()
receiveSync = do
    syncHandler <- socket Rep
    bind syncHandler "tcp://*:45554"
    forever $ do
        buffer <- receive syncHandler
        liftIO $ BC.putStrLn buffer
        let result = execRemoteProcedure $ readProcedure buffer
        case result of
            Nothing -> liftIO $ putStrLn "Sync Handler: Error"
            _ -> liftIO $ putStrLn "Sync Handler: Success"
        send syncHandler [] $ BC.pack (fromMaybe "Error" result)

main :: IO ()
main = runZMQ $ do
    async $ receiveAsync
    receiveSync
        