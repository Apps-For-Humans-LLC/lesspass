#ifndef _OSSL_PARAM_H_
#define _OSSL_PARAM_H_
#include "openssl/params.h"
OSSL_PARAM *make_pbkdf2_params(char *pass, char *salt);
void free_pbkdf2_params(OSSL_PARAM *params);
#endif
