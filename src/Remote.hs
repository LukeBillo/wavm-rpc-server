module Remote where


import qualified Data.ByteString.Char8 as BC
import Text.Read
import Data.Maybe
import WavmFFI
    
data RemoteCommand = Void String |
    InvalidCmd
    deriving (Read, Show)

data RemoteProcedure = Init String Bool |
    Execute String |
    InvalidProc
    deriving (Read, Show)

readCommand :: BC.ByteString -> RemoteCommand
readCommand bs = 
    let
        maybeCommand = readMaybe $ BC.unpack bs
    in
        case maybeCommand of
            (Just cmd) -> cmd
            Nothing -> InvalidCmd

readProcedure :: BC.ByteString -> RemoteProcedure
readProcedure bs = 
    let
        maybeProcedure = readMaybe $ BC.unpack bs
    in
        case maybeProcedure of
            (Just proc) -> proc
            Nothing -> InvalidProc

type CommandResult = Maybe ()
type ProcedureResult = Maybe String

execRemoteCommand :: RemoteCommand -> IO CommandResult
execRemoteCommand (Void function) = pure $ Just ()
execRemoteCommand InvalidCmd = pure Nothing

execRemoteProcedure :: RemoteProcedure -> IO ProcedureResult
execRemoteProcedure (Init wasmFile isPrecompiled) = do
                                                        putStrLn wasmFile
                                                        putStrLn $ show isPrecompiled
                                                        r <- initialiseWavm wasmFile isPrecompiled
                                                        case r of
                                                            True -> pure $ Just "str Initialised"
                                                            False -> pure Nothing

execRemoteProcedure (Execute function) = do
                                            putStrLn $ "Executing " ++ function
                                            r <- execute function
                                            pure $ Just r

execRemoteProcedure InvalidProc = pure Nothing