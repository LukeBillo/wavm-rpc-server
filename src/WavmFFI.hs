{-# LANGUAGE ForeignFunctionInterface #-}

module WavmFFI where

import System.IO.Unsafe
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

-- Foreign imports
foreign import ccall safe "runtime.h initialise" c_initialiseWavm :: CString -> CBool -> IO CBool
foreign import ccall safe "runtime.h execute" c_execute :: CString -> IO CString

toCBool :: Bool -> CBool
toCBool = fromBool

fromCBool :: CBool -> Bool
fromCBool = toBool

-- Haskell Wrappers
initialiseWavm :: String -> Bool -> IO Bool
initialiseWavm file isPrecompiled = do
    csFile <- newCString file
    isSuccessful <- c_initialiseWavm csFile (toCBool isPrecompiled)
    pure $ fromCBool isSuccessful

execute :: String -> IO String
execute func = do
    cStringFunc <- newCString func
    result <- c_execute cStringFunc
    s <- peekCString result
    putStrLn s
    pure s
