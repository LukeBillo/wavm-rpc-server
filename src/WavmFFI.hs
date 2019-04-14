{-# LANGUAGE ForeignFunctionInterface #-}

module WavmFFI where

import System.IO.Unsafe
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Control.Concurrent.MVar
import Control.Monad.Trans.State

-- Foreign imports
type WavmRuntimePtr = Ptr WavmRuntime
foreign import ccall unsafe "runtime.h newRuntime" c_newWavmRuntime :: IO WavmRuntimePtr
foreign import ccall unsafe "runtime.h initialise" c_initialiseWavm :: WavmRuntimePtr -> CString -> CBool -> IO CBool
foreign import ccall unsafe "runtime.h execute" c_execute :: WavmRuntimePtr -> CString -> IO CString

toCBool :: Bool -> CBool
toCBool = fromBool

fromCBool :: CBool -> Bool
fromCBool = toBool

data WavmRuntime = WavmRuntime
type GlobalWavmRuntime = WavmRuntimePtr

createNewWavmRuntime :: IO GlobalWavmRuntime
createNewWavmRuntime = c_newWavmRuntime


-- Haskell Wrappers
initialiseWavm :: GlobalWavmRuntime -> String -> Bool -> IO Bool
initialiseWavm rt file isPrecompiled = do
    csFile <- newCString file
    isSuccessful <- c_initialiseWavm rt csFile (toCBool isPrecompiled)
    pure $ fromCBool isSuccessful

execute :: GlobalWavmRuntime -> String -> IO String
execute rt func = do
    cStringFunc <- newCString func
    result <- c_execute rt cStringFunc
    s <- peekCString result
    putStrLn s
    pure s
