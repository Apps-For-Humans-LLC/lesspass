{-# LANGUAGE CApiFFI #-}

module PBKDF2 () where

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

withKdf :: String -> (Ptr EVP_KDF -> IO a) -> IO a
withKdf algo fn = withCString algo $ \cstr ->
  bracket
    (c_EVP_KDF_fetch nullPtr cstr nullPtr)
    c_EVP_KDF_free
    fn

withKdfCtx :: Ptr EVP_KDF -> (Ptr EVP_KDF_CTX -> IO a) -> IO a
withKdfCtx kdf = bracket (c_EVP_KDF_CTX_new kdf) c_EVP_KDF_CTX_free
