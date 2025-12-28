#include "ossl_param.h"
#include <openssl/core.h>
#include <openssl/core_names.h>
#include <openssl/kdf.h>
#include <openssl/params.h>
#include <stdlib.h>
#include <string.h>

OSSL_PARAM *make_pbkdf2_params(char *pass, char *salt) {
  OSSL_PARAM *out = (OSSL_PARAM *)calloc(5, sizeof(OSSL_PARAM));
  unsigned int *iter = malloc(sizeof(unsigned int));
  *iter = 100000;
  if (out && iter) {
    out[0] = OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_PASSWORD, pass,
                                               strlen(pass));
    out[1] = OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_SALT, salt,
                                               strlen(salt));
    out[2] = OSSL_PARAM_construct_uint(OSSL_KDF_PARAM_ITER, iter);
    out[3] =
        OSSL_PARAM_construct_utf8_string(OSSL_KDF_PARAM_DIGEST, "SHA256", 0);
    out[4] = OSSL_PARAM_construct_end();
    return out;
  }

  if (iter)
    free(iter);
  if (out)
    free(out);
  return NULL;
}

void free_pbkdf2_params(OSSL_PARAM *params) {
  if (params) {
    if (params[2].data) {
      free(params[2].data);
    }
    free(params);
  }
}
