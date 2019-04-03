module Remote where

import qualified Data.ByteString.Char8 as BC
import Text.Read
import Data.Maybe
    
data RemoteCommand = Init String |
    Void String |
    InvalidCmd
    deriving (Read, Show)

data RemoteProcedure = Execute String |
    InvalidProc
    deriving (Read, Show)

takeRpcResult :: Either CommandResult ProcedureResult -> String
takeRpcResult (Right maybeValue) = fromMaybe "Error" maybeValue
takeRpcResult (Left maybeSuccess) | maybeSuccess == Nothing = "Error"
                                  | otherwise = "Success"

run :: BC.ByteString -> Either CommandResult ProcedureResult
run bs = 
    let
        rpc = readRpc bs
    in
        case rpc of
            (Left cmd) -> Left (execRemoteCommand cmd)
            (Right proc) -> Right (execRemoteProcedure proc)

readRpc :: BC.ByteString -> Either RemoteCommand RemoteProcedure
readRpc bs = 
    let
        cmd = readCommand bs
        proc = readProcedure bs
    in
        case (cmd, proc) of
            (_, InvalidProc) -> Left cmd
            (InvalidCmd, _) -> Right proc

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

execRemoteCommand :: RemoteCommand -> CommandResult
execRemoteCommand (Init wasmFile) = Just ()
execRemoteCommand (Void function) = Just ()
execRemoteCommand InvalidCmd = Nothing

execRemoteProcedure :: RemoteProcedure -> ProcedureResult
execRemoteProcedure (Execute function) = Just "1 i32"
execRemoteProcedure InvalidProc = Nothing