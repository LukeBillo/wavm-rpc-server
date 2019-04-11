module Remote where


import qualified Data.ByteString.Char8 as BC
import Text.Read
import Data.Maybe
import WavmFFI
    
data RemoteCommand = Init String |
    Void String |
    InvalidCmd
    deriving (Read, Show)

data RemoteProcedure = Execute String |
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
execRemoteCommand (Init wasmFile) = do
                                        r <- initialiseWavm wasmFile False
                                        case r of
                                            0 -> pure $ Just ()
                                            _ -> pure $ Nothing

execRemoteCommand (Void function) = pure $ Just ()
execRemoteCommand InvalidCmd = pure Nothing

execRemoteProcedure :: RemoteProcedure -> ProcedureResult
execRemoteProcedure (Execute function) = Just "i32 1"
execRemoteProcedure InvalidProc = Nothing