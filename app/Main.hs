{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import System.ZMQ4.Monadic
import Remote
import Data.Maybe
import WavmFFI

import qualified Data.ByteString.Char8 as BC

receiveAsync :: GlobalWavmRuntime -> ZMQ z ()
receiveAsync gwr = do
    asyncHandler <- socket Pull
    bind asyncHandler "tcp://*:45555"
    forever $ do
        buffer <- receive asyncHandler
        liftIO $ BC.putStrLn buffer
        result <- liftIO $ execRemoteCommand gwr $ readCommand buffer
        case result of
            Nothing -> liftIO $ putStrLn "Async Handler: Error"
            _ -> liftIO $ putStrLn "Async Handler: Success"

receiveSync :: GlobalWavmRuntime -> ZMQ z ()
receiveSync gwr = do
    syncHandler <- socket Rep
    bind syncHandler "tcp://*:45554"
    forever $ do
        buffer <- receive syncHandler
        liftIO $ BC.putStrLn buffer
        result <- liftIO $ execRemoteProcedure gwr $ readProcedure buffer
        case result of
            Nothing -> liftIO $ putStrLn "Sync Handler: Error"
            _ ->  liftIO $ putStrLn "Sync Handler: Success"
        send syncHandler [] $ BC.pack (fromMaybe "Error" result)

main :: IO ()
main = do
    runtime <- createNewWavmRuntime
    isSuccessful <- initialiseWavm runtime "/home/luke/Documents/c++-wasm-files/wasm/basic-functions-2.wasm" False
    r <- execute runtime "_getMyNumber"
    putStrLn r
    runZMQ $ do
        --async $ receiveAsync runtime
        receiveSync runtime