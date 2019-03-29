module Remote where

import qualified Data.ByteString.Char8 as BC
    
data RemoteCommand = Init String |
    Execute String |
    Void String |
    Invalid 
    deriving (Read, Show)

readCommand :: BC.ByteString -> RemoteCommand
readCommand = read . BC.unpack

execRemoteCommand :: RemoteCommand -> IO ()
execRemoteCommand (Init wasmFile) = putStrLn $ "Init " ++ wasmFile ++ " executed"
execRemoteCommand (Execute function) = putStrLn $ "Execute " ++ function ++ " executed"
execRemoteCommand (Void function) = putStrLn $ "Void " ++ function ++ " executed"
execRemoteCommand Invalid = putStrLn "Invalid function received"