#include "openssl/params.h"
#include <stdlib.h>

OSSL_PARAM *ffi_OSSL_PARAM_construct_utf8_string(const char *key, char *buf) {
  OSSL_PARAM *p = (OSSL_PARAM *)malloc(sizeof(OSSL_PARAM));
  if (p) {
    *p = OSSL_PARAM_construct_utf8_string(key, buf, 0);
  }
  return p;
}

OSSL_PARAM *ffi_OSSL_PARAM_construct_octet_string(const char *key, char *buf,
                                                  size_t len) {
  OSSL_PARAM *p = (OSSL_PARAM *)malloc(sizeof(OSSL_PARAM));
  if (p) {
    *p = OSSL_PARAM_construct_octet_string(key, buf, len);
  }
  return p;
}

OSSL_PARAM *ffi_combine_ossl_params(int n, OSSL_PARAM **ptrs) {
  OSSL_PARAM *array = malloc(sizeof(OSSL_PARAM) * n);
  if (array) {
    for (int i = 0; i < n; i++) {
      array[i] = *ptrs[i];
    }
  }
  return array;
}
