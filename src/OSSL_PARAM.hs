{-# LANGUAGE CApiFFI #-}

module OSSL_PARAM () where

import Foreign.C (CSize (..), CString, withCString)
import Foreign.Ptr (Ptr)

data {-# CTYPE "openssl/core.h" "OSSL_PARAM" #-} OSSL_PARAM

foreign import capi "ossl_param.h ffi_OSSL_PARAM_construct_utf8_string" c_OSSL_PARAM_construct_utf8_string :: CString -> CString -> IO (Ptr OSSL_PARAM)

foreign import capi "ossl_param.h ffi_OSSL_PARAM_construct_octet_string" c_OSSL_PARAM_construct_octet_string :: CString -> CString -> CSize -> IO (Ptr OSSL_PARAM)

-- | Create an OSSL_PARAM from a string
osslParamUtf8Str :: String -> String -> IO (Ptr OSSL_PARAM)
osslParamUtf8Str key value =
  withCString key $ \key' ->
    withCString value $ \value' ->
      c_OSSL_PARAM_construct_utf8_string key' value'

osslParamOctetStr :: String -> String -> IO (Ptr OSSL_PARAM)
osslParamOctetStr key value =
  withCString key $ \key' ->
    withCString value $ \value' ->
      c_OSSL_PARAM_construct_octet_string key' value' ((fromIntegral . length) value)
