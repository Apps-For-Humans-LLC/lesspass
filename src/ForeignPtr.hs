module ForeignPtr (errorIfNull) where

import Foreign.Ptr (Ptr, nullPtr)

-- | Throws an error if the given pointer is NULL else just returns the pointer
errorIfNull :: Ptr a -> Ptr a
errorIfNull p
  | p == nullPtr = error "Null Pointer Exception"
  | otherwise = p
