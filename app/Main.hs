{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import System.ZMQ4.Monadic
import Remote
import Data.Maybe
import WavmFFI

import qualified Data.ByteString.Char8 as BC

runBundledCommands :: GlobalWavmRuntime -> Commands -> IO ()
runBundledCommands _ [] = pure ()
runBundledCommands gwr (c:cs) = do
    result <- execRemoteCommand gwr c
    case result of
        Nothing -> liftIO $ putStrLn "Bundled Async Handler: Error"
        _ -> liftIO $ putStrLn "Bundled Async Handler: Success"
    runBundledCommands gwr cs

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
        let bundle = splitBundle $ readBundle buffer
        case bundle of
            Nothing -> do
                liftIO $ putStrLn "Sync Handler: Broken bundle received"
                send syncHandler [] $ BC.pack "Error"
                
            Just (bundleParts) -> do
                _ <- async $ liftIO $ runBundledCommands gwr (fst bundleParts)
                result <- liftIO $ execRemoteProcedure gwr (snd bundleParts)
                case result of
                    Nothing -> liftIO $ putStrLn "Sync Handler: Error"
                    _ ->  liftIO $ putStrLn "Sync Handler: Success"
                send syncHandler [] $ BC.pack (fromMaybe "Error" result)

main :: IO ()
main = do
    runtime <- createNewWavmRuntime
    runZMQ $ do
        _ <- async $ receiveAsync runtime
        receiveSync runtime