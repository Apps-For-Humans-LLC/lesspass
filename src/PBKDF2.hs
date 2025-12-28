{-# LANGUAGE CApiFFI #-}

module PBKDF2
  ( withKdf,
    withKdfCtx,
    withPbkdf2Params,
    pbkdf2,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM)
import Data.Bits (shiftL, (.|.))
import Data.Word (Word8)
import Foreign.C (CInt (..), CSize (..), CString, CUChar, withCString)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peekByteOff)
import ForeignPtr (errorIfNull)

data {-# CTYPE "openssl/kdf.h" "EVP_KDF_CTX" #-} EVP_KDF_CTX

data {-# CTYPE "openssl/kdf.h" "EVP_KDF" #-} EVP_KDF

data {-# CTYPE "openssl/core.h" "OSSL_PARAM" #-} OSSL_PARAM

data OSSL_LIB_CTX

foreign import capi "openssl/kdf.h EVP_KDF_CTX_new" c_EVP_KDF_CTX_new :: Ptr EVP_KDF -> IO (Ptr EVP_KDF_CTX)

foreign import capi "openssl/kdf.h EVP_KDF_CTX_free" c_EVP_KDF_CTX_free :: Ptr EVP_KDF_CTX -> IO ()

foreign import capi "openssl/kdf.h EVP_KDF_fetch" c_EVP_KDF_fetch :: Ptr OSSL_LIB_CTX -> CString -> CString -> IO (Ptr EVP_KDF)

foreign import capi "openssl/kdf.h EVP_KDF_free" c_EVP_KDF_free :: Ptr EVP_KDF -> IO ()

foreign import capi "ossl_param.h make_pbkdf2_params" c_make_pbkdf2_params :: CString -> CString -> IO (Ptr OSSL_PARAM)

foreign import capi "ossl_param.h free_pbkdf2_params" c_free_pbkdf2_params :: Ptr OSSL_PARAM -> IO ()

foreign import capi "openssl/kdf.h EVP_KDF_derive" c_EVP_KDF_derive :: Ptr EVP_KDF_CTX -> Ptr CUChar -> CSize -> Ptr OSSL_PARAM -> IO (CInt)

withPbkdf2Params :: String -> String -> (Ptr OSSL_PARAM -> IO a) -> IO a
withPbkdf2Params pass salt fn =
  withCString pass $ \cpass ->
    withCString salt $ \csalt ->
      bracket
        (c_make_pbkdf2_params cpass csalt)
        c_free_pbkdf2_params
        fn

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

bytesToInteger :: Ptr Word8 -> Int -> IO Integer
bytesToInteger mem size = do
  bytes <- forM [0 .. size - 1] $ \i -> peekByteOff mem i :: IO CUChar
  return $ foldl (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) 0 bytes

pbkdf2 :: String -> String -> IO Integer
pbkdf2 pass val = withKdfCtx "pbkdf2" $ \ctx ->
  withPbkdf2Params pass val $ \params ->
    allocaBytes 32 $ \mem -> do
      result <- c_EVP_KDF_derive ctx mem 32 params
      if result == 0
        then error "PBKDF2 command failed"
        else bytesToInteger (castPtr mem) 32
