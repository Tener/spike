{-# LANGUAGE PackageImports, FlexibleInstances, DoRec, ForeignFunctionInterface #-}

module CFunctions(spikeSetupWebkitGlobals) where

import Foreign
import Foreign.C

foreign import ccall "spike_setup_webkit_globals" spike_setup_webkit_globals :: CString -> CString -> IO ()

spikeSetupWebkitGlobals arg1 arg2 =
  withCString arg1 $ \ arg1' -> 
  withCString arg2 $ \ arg2' -> spike_setup_webkit_globals arg1' arg2'
