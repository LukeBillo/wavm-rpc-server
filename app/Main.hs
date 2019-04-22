{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import System.ZMQ4.Monadic
import Remote
import Data.Maybe
import WavmFFI
import Utility

import qualified Data.ByteString.Char8 as BC

runBundledCommands :: GlobalWavmRuntime -> Commands -> IO ()
runBundledCommands _ [] = pure ()
runBundledCommands gwr (c:cs) = do
    result <- execRemoteCommand gwr c
    case result of
        Nothing -> liftIO $ putStrLn "Command Handler: Error"
        _ -> liftIO $ putStrLn "Command Handler: Success"
    runBundledCommands gwr cs

receiveAsync :: GlobalWavmRuntime -> ZMQ z ()
receiveAsync gwr = do
    asyncHandler <- socket Pull
    bind asyncHandler "tcp://*:45555"
    forever $ do
        buffer <- receive asyncHandler
        liftIO $ BC.putStrLn buffer
        liftIO $ runBundledCommands gwr $ readCommands buffer

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
                liftIO $ putStrLn "Procedure Handler: Broken bundle received"
                send syncHandler [] $ BC.pack "Error"
                
            Just (bundleParts) -> do
                liftIO $ runBundledCommands gwr (fst bundleParts)
                result <- liftIO $ execRemoteProcedure gwr (snd bundleParts)
                case result of
                    Nothing -> liftIO $ putStrLn "Procedure Handler: Error"
                    _ ->  liftIO $ putStrLn "Procedure Handler: Success"
                send syncHandler [] $ BC.pack $ cleanWavmResult (fromMaybe "Error" result)

main :: IO ()
main = do
    runtime <- createNewWavmRuntime
    runZMQ $ do
        _ <- async $ receiveAsync runtime
        receiveSync runtime