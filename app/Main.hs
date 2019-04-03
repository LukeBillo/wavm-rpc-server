{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import System.ZMQ4.Monadic
import Remote
import Data.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = runZMQ $ do
    -- Socket to talk to clients
    responder <- socket Rep
    bind responder "tcp://*:45555"
    forever $ do
        buffer <- receive responder
        liftIO $ B.putStrLn buffer
        let result = takeRpcResult $ run buffer
        send responder [] $ BC.pack result