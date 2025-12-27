{-# LANGUAGE CApiFFI #-}

module PBKDF2
  ( withKdf,
    withKdfCtx,
  )
where

import Control.Exception (bracket)
import Foreign.C (CString, withCString)
import Foreign.Ptr (Ptr, nullPtr)

data {-# CTYPE "openssl/kdf.h" "EVP_KDF_CTX" #-} EVP_KDF_CTX

data {-# CTYPE "openssl/kdf.h" "EVP_KDF" #-} EVP_KDF

data OSSL_LIB_CTX

foreign import capi "openssl/kdf.h EVP_KDF_CTX_new" c_EVP_KDF_CTX_new :: Ptr EVP_KDF -> IO (Ptr EVP_KDF_CTX)

foreign import capi "openssl/kdf.h EVP_KDF_CTX_free" c_EVP_KDF_CTX_free :: Ptr EVP_KDF_CTX -> IO ()

foreign import capi "openssl/kdf.h EVP_KDF_fetch" c_EVP_KDF_fetch :: Ptr OSSL_LIB_CTX -> CString -> CString -> IO (Ptr EVP_KDF)

foreign import capi "openssl/kdf.h EVP_KDF_free" c_EVP_KDF_free :: Ptr EVP_KDF -> IO ()

-- | Throws an error if the given pointer is NULL else just returns the pointer
errorIfNull :: Ptr a -> Ptr a
errorIfNull p
  | p == nullPtr = error "Null Pointer Exception"
  | otherwise = p

-- | Performs an action with the given 'EVP_KDF'
withKdf :: String -> (Ptr EVP_KDF -> IO a) -> IO a
withKdf algo fn = withCString algo $ \cstr ->
  bracket
    (c_EVP_KDF_fetch nullPtr cstr nullPtr)
    c_EVP_KDF_free
    (fn . errorIfNull)

-- | Performs an action with the given 'EVP_KDF_CTX'
withKdfCtx' :: Ptr EVP_KDF -> (Ptr EVP_KDF_CTX -> IO a) -> IO a
withKdfCtx' kdf fn =
  bracket
    (c_EVP_KDF_CTX_new kdf)
    c_EVP_KDF_CTX_free
    (fn . errorIfNull)

-- | Performs an action with the given KDF algorithm provided as a string
-- |
-- | See https://docs.openssl.org/master/man3/EVP_KDF/
withKdfCtx :: String -> (Ptr EVP_KDF_CTX -> IO a) -> IO a
withKdfCtx algo fn = withKdf algo (`withKdfCtx'` fn)
