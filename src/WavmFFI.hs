{-# LANGUAGE ForeignFunctionInterface #-}

module WavmFFI where

import System.IO.Unsafe
import Foreign.C.Types
import Foreign.C.String
import Unsafe.Coerce

-- Foreign imports
foreign import ccall safe "runtime.h initialise" c_initialiseWavm :: CString -> CBool -> IO CInt

toCBool :: Bool -> CBool
toCBool = unsafeCoerce

fromCInt :: IO CInt -> IO Int
fromCInt ioc = do
    cInt <- ioc
    unsafeCoerce cInt

-- Haskell Wrappers
initialiseWavm :: String -> Bool -> IO Int
initialiseWavm file isPrecompiled = do
    csFile <- newCStringLen file
    fromCInt $ c_initialiseWavm (fst csFile) (toCBool isPrecompiled)