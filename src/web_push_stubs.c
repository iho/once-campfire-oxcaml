#include <limits.h>
#include <openssl/bn.h>
#include <openssl/core_names.h>
#include <openssl/ec.h>
#include <openssl/ecdsa.h>
#include <openssl/evp.h>
#include <openssl/param_build.h>
#include <openssl/params.h>
#include <openssl/rand.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static void fail_push_crypto(void) {
  caml_failwith("Web Push cryptographic operation failed");
}

static int p256_public_from_scalar(const unsigned char scalar[32],
                                   unsigned char encoded[65]) {
  int ok = 0;
  EC_GROUP *group = EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1);
  EC_POINT *point = NULL;
  BIGNUM *private_value = BN_bin2bn(scalar, 32, NULL);
  BIGNUM *order = BN_new();
  if (group != NULL && private_value != NULL && order != NULL &&
      EC_GROUP_get_order(group, order, NULL) == 1 && !BN_is_zero(private_value) &&
      BN_cmp(private_value, order) < 0) {
    point = EC_POINT_new(group);
    ok = point != NULL &&
         EC_POINT_mul(group, point, private_value, NULL, NULL, NULL) == 1 &&
         EC_POINT_point2oct(group, point, POINT_CONVERSION_UNCOMPRESSED, encoded,
                            65, NULL) == 65;
  }
  EC_POINT_free(point);
  BN_clear_free(private_value);
  BN_free(order);
  EC_GROUP_free(group);
  return ok;
}

static EVP_PKEY *p256_import(const unsigned char scalar[32],
                             const unsigned char public_bytes[65]) {
  EVP_PKEY *key = NULL;
  EVP_PKEY_CTX *ctx = NULL;
  OSSL_PARAM_BLD *builder = OSSL_PARAM_BLD_new();
  OSSL_PARAM *params = NULL;
  BIGNUM *private_value = BN_bin2bn(scalar, 32, NULL);
  if (builder != NULL && private_value != NULL &&
      OSSL_PARAM_BLD_push_utf8_string(builder, OSSL_PKEY_PARAM_GROUP_NAME,
                                      "prime256v1", 0) == 1 &&
      OSSL_PARAM_BLD_push_BN(builder, OSSL_PKEY_PARAM_PRIV_KEY,
                             private_value) == 1 &&
      OSSL_PARAM_BLD_push_octet_string(builder, OSSL_PKEY_PARAM_PUB_KEY,
                                       public_bytes, 65) == 1) {
    params = OSSL_PARAM_BLD_to_param(builder);
    ctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL);
    if (params != NULL && ctx != NULL && EVP_PKEY_fromdata_init(ctx) == 1 &&
        EVP_PKEY_fromdata(ctx, &key, EVP_PKEY_KEYPAIR, params) != 1) {
      EVP_PKEY_free(key);
      key = NULL;
    }
  }
  EVP_PKEY_CTX_free(ctx);
  OSSL_PARAM_free(params);
  OSSL_PARAM_BLD_free(builder);
  BN_clear_free(private_value);
  return key;
}

static EVP_PKEY *p256_public_import(const unsigned char encoded[65]) {
  EVP_PKEY *key = NULL;
  EVP_PKEY_CTX *ctx = NULL;
  OSSL_PARAM_BLD *builder = OSSL_PARAM_BLD_new();
  OSSL_PARAM *params = NULL;
  EC_GROUP *group = EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1);
  EC_POINT *point = group == NULL ? NULL : EC_POINT_new(group);
  int valid_point = encoded[0] == 4 && group != NULL && point != NULL &&
                    EC_POINT_oct2point(group, point, encoded, 65, NULL) == 1 &&
                    EC_POINT_is_at_infinity(group, point) == 0 &&
                    EC_POINT_is_on_curve(group, point, NULL) == 1;
  if (valid_point && builder != NULL &&
      OSSL_PARAM_BLD_push_utf8_string(builder, OSSL_PKEY_PARAM_GROUP_NAME,
                                      "prime256v1", 0) == 1 &&
      OSSL_PARAM_BLD_push_octet_string(builder, OSSL_PKEY_PARAM_PUB_KEY,
                                       encoded, 65) == 1) {
    params = OSSL_PARAM_BLD_to_param(builder);
    ctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL);
    if (params != NULL && ctx != NULL && EVP_PKEY_fromdata_init(ctx) == 1 &&
        EVP_PKEY_fromdata(ctx, &key, EVP_PKEY_PUBLIC_KEY, params) != 1) {
      EVP_PKEY_free(key);
      key = NULL;
    }
  }
  EC_POINT_free(point);
  EC_GROUP_free(group);
  EVP_PKEY_CTX_free(ctx);
  OSSL_PARAM_free(params);
  OSSL_PARAM_BLD_free(builder);
  return key;
}

static EVP_PKEY *p256_private_import(const unsigned char scalar[32]) {
  unsigned char public_bytes[65];
  if (!p256_public_from_scalar(scalar, public_bytes)) return NULL;
  EVP_PKEY *key = p256_import(scalar, public_bytes);
  OPENSSL_cleanse(public_bytes, sizeof(public_bytes));
  return key;
}

CAMLprim value campfire_web_push_generate_keypair(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(result, private_value, public_value);
  EVP_PKEY_CTX *ctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL);
  EVP_PKEY *key = NULL;
  unsigned char private_bytes[32], public_bytes[65];
  size_t public_length = sizeof(public_bytes);
  BIGNUM *private_bn = NULL;
  OSSL_PARAM params[] = {
      OSSL_PARAM_utf8_string(OSSL_PKEY_PARAM_GROUP_NAME, "prime256v1", 0),
      OSSL_PARAM_END};
  int ok = ctx != NULL && EVP_PKEY_keygen_init(ctx) == 1 &&
           EVP_PKEY_CTX_set_params(ctx, params) == 1 &&
           EVP_PKEY_generate(ctx, &key) == 1 &&
           EVP_PKEY_get_bn_param(key, OSSL_PKEY_PARAM_PRIV_KEY, &private_bn) == 1 &&
           BN_bn2binpad(private_bn, private_bytes, 32) == 32 &&
           EVP_PKEY_get_octet_string_param(key, OSSL_PKEY_PARAM_PUB_KEY,
                                           public_bytes, sizeof(public_bytes),
                                           &public_length) == 1 &&
           public_length == sizeof(public_bytes);
  EVP_PKEY_CTX_free(ctx);
  EVP_PKEY_free(key);
  BN_clear_free(private_bn);
  if (!ok) {
    OPENSSL_cleanse(private_bytes, sizeof(private_bytes));
    OPENSSL_cleanse(public_bytes, sizeof(public_bytes));
    fail_push_crypto();
  }
  private_value = caml_alloc_string(sizeof(private_bytes));
  public_value = caml_alloc_string(sizeof(public_bytes));
  memcpy(Bytes_val(private_value), private_bytes, sizeof(private_bytes));
  memcpy(Bytes_val(public_value), public_bytes, sizeof(public_bytes));
  result = caml_alloc_tuple(2);
  Store_field(result, 0, private_value);
  Store_field(result, 1, public_value);
  OPENSSL_cleanse(private_bytes, sizeof(private_bytes));
  OPENSSL_cleanse(public_bytes, sizeof(public_bytes));
  CAMLreturn(result);
}

CAMLprim value campfire_web_push_ecdh(value private_value, value public_value) {
  CAMLparam2(private_value, public_value);
  CAMLlocal1(shared);
  if (caml_string_length(private_value) != 32 ||
      caml_string_length(public_value) != 65)
    caml_invalid_argument("invalid P-256 key");
  EVP_PKEY *key = p256_private_import(
      (const unsigned char *)String_val(private_value));
  EVP_PKEY *peer = p256_public_import(
      (const unsigned char *)String_val(public_value));
  EVP_PKEY_CTX *ctx = key == NULL ? NULL : EVP_PKEY_CTX_new_from_pkey(NULL, key, NULL);
  unsigned char secret[32];
  size_t secret_length = sizeof(secret);
  int ok = ctx != NULL && peer != NULL && EVP_PKEY_derive_init(ctx) == 1 &&
           EVP_PKEY_derive_set_peer(ctx, peer) == 1 &&
           EVP_PKEY_derive(ctx, secret, &secret_length) == 1 &&
           secret_length == sizeof(secret);
  EVP_PKEY_CTX_free(ctx);
  EVP_PKEY_free(key);
  EVP_PKEY_free(peer);
  if (!ok) {
    OPENSSL_cleanse(secret, sizeof(secret));
    fail_push_crypto();
  }
  shared = caml_alloc_string(sizeof(secret));
  memcpy(Bytes_val(shared), secret, sizeof(secret));
  OPENSSL_cleanse(secret, sizeof(secret));
  CAMLreturn(shared);
}

CAMLprim value campfire_web_push_aes128gcm(value key, value nonce, value data) {
  CAMLparam3(key, nonce, data);
  CAMLlocal1(output);
  EVP_CIPHER_CTX *ctx = NULL;
  int written = 0, final_written = 0;
  mlsize_t data_len = caml_string_length(data);
  if (caml_string_length(key) != 16 || caml_string_length(nonce) != 12 ||
      data_len > INT_MAX) caml_invalid_argument("invalid AES-128-GCM input");
  output = caml_alloc_string(data_len + 16);
  ctx = EVP_CIPHER_CTX_new();
  if (ctx == NULL ||
      EVP_EncryptInit_ex(ctx, EVP_aes_128_gcm(), NULL, NULL, NULL) != 1 ||
      EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, 12, NULL) != 1 ||
      EVP_EncryptInit_ex(ctx, NULL, NULL,
                         (const unsigned char *)String_val(key),
                         (const unsigned char *)String_val(nonce)) != 1 ||
      EVP_EncryptUpdate(ctx, (unsigned char *)Bytes_val(output), &written,
                        (const unsigned char *)String_val(data),
                        (int)data_len) != 1 ||
      EVP_EncryptFinal_ex(ctx, (unsigned char *)Bytes_val(output) + written,
                          &final_written) != 1 ||
      EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_GET_TAG, 16,
                          (unsigned char *)Bytes_val(output) + data_len) != 1) {
    EVP_CIPHER_CTX_free(ctx);
    fail_push_crypto();
  }
  EVP_CIPHER_CTX_free(ctx);
  if ((mlsize_t)(written + final_written) != data_len) fail_push_crypto();
  CAMLreturn(output);
}

CAMLprim value campfire_web_push_vapid_sign(value private_value, value message) {
  CAMLparam2(private_value, message);
  CAMLlocal1(output);
  if (caml_string_length(private_value) != 32)
    caml_invalid_argument("invalid VAPID private key");
  EVP_PKEY *key = p256_private_import(
      (const unsigned char *)String_val(private_value));
  EVP_MD_CTX *ctx = EVP_MD_CTX_new();
  unsigned char *der_signature = NULL;
  size_t der_length = 0;
  int ok = key != NULL && ctx != NULL &&
           EVP_DigestSignInit(ctx, NULL, EVP_sha256(), NULL, key) == 1 &&
           EVP_DigestSign(ctx, NULL, &der_length,
                          (const unsigned char *)String_val(message),
                          caml_string_length(message)) == 1 &&
           der_length > 0 && der_length <= 80;
  if (ok) der_signature = OPENSSL_malloc(der_length);
  if (ok && der_signature != NULL)
    ok = EVP_DigestSign(ctx, der_signature, &der_length,
                        (const unsigned char *)String_val(message),
                        caml_string_length(message)) == 1;
  const unsigned char *cursor = der_signature;
  ECDSA_SIG *signature = ok ? d2i_ECDSA_SIG(NULL, &cursor, der_length) : NULL;
  const BIGNUM *r = NULL, *s = NULL;
  unsigned char raw_signature[64];
  if (signature != NULL) {
    ECDSA_SIG_get0(signature, &r, &s);
    ok = BN_bn2binpad(r, raw_signature, 32) == 32 &&
         BN_bn2binpad(s, raw_signature + 32, 32) == 32;
  } else {
    ok = 0;
  }
  ECDSA_SIG_free(signature);
  EVP_MD_CTX_free(ctx);
  EVP_PKEY_free(key);
  OPENSSL_clear_free(der_signature, der_length);
  if (!ok) {
    OPENSSL_cleanse(raw_signature, sizeof(raw_signature));
    fail_push_crypto();
  }
  output = caml_alloc_string(sizeof(raw_signature));
  memcpy(Bytes_val(output), raw_signature, sizeof(raw_signature));
  OPENSSL_cleanse(raw_signature, sizeof(raw_signature));
  CAMLreturn(output);
}

CAMLprim value campfire_web_push_public_from_private(value private_value) {
  CAMLparam1(private_value);
  CAMLlocal1(output);
  if (caml_string_length(private_value) != 32)
    caml_invalid_argument("invalid P-256 private key");
  unsigned char public_bytes[65];
  if (!p256_public_from_scalar((const unsigned char *)String_val(private_value),
                               public_bytes))
    fail_push_crypto();
  output = caml_alloc_string(sizeof(public_bytes));
  memcpy(Bytes_val(output), public_bytes, sizeof(public_bytes));
  OPENSSL_cleanse(public_bytes, sizeof(public_bytes));
  CAMLreturn(output);
}

CAMLprim value campfire_web_push_vapid_verify(value public_value, value message,
                                             value signature_value) {
  CAMLparam3(public_value, message, signature_value);
  if (caml_string_length(public_value) != 65 ||
      caml_string_length(signature_value) != 64) CAMLreturn(Val_false);
  EVP_PKEY *key = p256_public_import(
      (const unsigned char *)String_val(public_value));
  ECDSA_SIG *signature = ECDSA_SIG_new();
  BIGNUM *r = BN_bin2bn((const unsigned char *)String_val(signature_value), 32, NULL);
  BIGNUM *s = BN_bin2bn((const unsigned char *)String_val(signature_value) + 32,
                        32, NULL);
  int valid = key != NULL && signature != NULL && r != NULL && s != NULL &&
              ECDSA_SIG_set0(signature, r, s) == 1;
  if (valid) {
    r = NULL;
    s = NULL;
    int der_length = i2d_ECDSA_SIG(signature, NULL);
    unsigned char *der = der_length > 0 ? OPENSSL_malloc(der_length) : NULL;
    unsigned char *cursor = der;
    valid = der != NULL && i2d_ECDSA_SIG(signature, &cursor) == der_length;
    EVP_MD_CTX *ctx = valid ? EVP_MD_CTX_new() : NULL;
    valid = valid && ctx != NULL &&
            EVP_DigestVerifyInit(ctx, NULL, EVP_sha256(), NULL, key) == 1 &&
            EVP_DigestVerify(ctx, der, der_length,
                             (const unsigned char *)String_val(message),
                             caml_string_length(message)) == 1;
    EVP_MD_CTX_free(ctx);
    OPENSSL_clear_free(der, der_length > 0 ? (size_t)der_length : 0);
  }
  BN_clear_free(r);
  BN_clear_free(s);
  ECDSA_SIG_free(signature);
  EVP_PKEY_free(key);
  CAMLreturn(Val_bool(valid));
}
