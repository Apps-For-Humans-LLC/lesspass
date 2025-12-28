#ifndef _OSSL_PARAM_H_
#define _OSSL_PARAM_H_
#include "openssl/params.h"

OSSL_PARAM *ffi_OSSL_PARAM_construct_utf8_string(const char *key, char *buf);
OSSL_PARAM *ffi_OSSL_PARAM_construct_octet_string(const char *key, char *buf,
                                                  size_t len);
#endif
