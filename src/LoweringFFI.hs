module LoweringFFI where

import Prelude
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = fromIntegral . fromEnum




data Module

foreign import ccall "lowering.h mkModule" mkModule :: IO (Ptr Module)
foreign import ccall "lowering.h getModuleString" getModuleString_ :: Ptr Module -> IO CString

getModuleString :: Ptr Module -> IO String
getModuleString p = getModuleString_ p >>= peekCString
