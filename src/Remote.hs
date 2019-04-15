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

execRemoteCommand :: GlobalWavmRuntime -> RemoteCommand -> IO CommandResult
execRemoteCommand gwr (Void function) = do
                                        _ <- execute gwr function
                                        pure $ Just ()
                                        
execRemoteCommand gwr InvalidCmd = pure Nothing

execRemoteProcedure :: GlobalWavmRuntime -> RemoteProcedure -> IO ProcedureResult
execRemoteProcedure gwr (Init wasmFile isPrecompiled) = do
                                                        r <- initialiseWavm gwr wasmFile isPrecompiled
                                                        case r of
                                                            True -> pure $ Just "str Initialised"
                                                            False -> pure Nothing

execRemoteProcedure gwr (Execute function) = do
                                            r <- execute gwr function
                                            pure $ Just r

execRemoteProcedure gwr InvalidProc = pure Nothing