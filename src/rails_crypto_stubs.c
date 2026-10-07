#include <limits.h>
#include <openssl/crypto.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <openssl/rand.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static void fail_crypto(void) { caml_failwith("native crypto operation failed"); }

CAMLprim value campfire_pbkdf2_sha256(value password, value salt,
                                       value iterations, value length) {
  CAMLparam4(password, salt, iterations, length);
  CAMLlocal1(output);
  int rounds = Int_val(iterations);
  int out_len = Int_val(length);
  mlsize_t password_len = caml_string_length(password);
  mlsize_t salt_len = caml_string_length(salt);

  if (rounds <= 0 || out_len <= 0 || out_len > 1024 ||
      password_len > INT_MAX || salt_len > INT_MAX) {
    caml_invalid_argument("invalid PBKDF2 parameters");
  }
  output = caml_alloc_string(out_len);
  if (PKCS5_PBKDF2_HMAC(String_val(password), (int)password_len,
                        (const unsigned char *)String_val(salt),
                        (int)salt_len, rounds, EVP_sha256(), out_len,
                        (unsigned char *)Bytes_val(output)) != 1) {
    fail_crypto();
  }
  CAMLreturn(output);
}

CAMLprim value campfire_hmac(value algorithm, value key, value data) {
  CAMLparam3(algorithm, key, data);
  CAMLlocal1(output);
  const EVP_MD *digest = Int_val(algorithm) == 1 ? EVP_sha1() : EVP_sha256();
  unsigned char bytes[EVP_MAX_MD_SIZE];
  unsigned int length = 0;
  mlsize_t key_len = caml_string_length(key);
  mlsize_t data_len = caml_string_length(data);

  if (key_len > INT_MAX || data_len > INT_MAX ||
      HMAC(digest, String_val(key), (int)key_len,
           (const unsigned char *)String_val(data), data_len, bytes,
           &length) == NULL) {
    fail_crypto();
  }
  output = caml_alloc_string(length);
  memcpy(Bytes_val(output), bytes, length);
  OPENSSL_cleanse(bytes, sizeof(bytes));
  CAMLreturn(output);
}

CAMLprim value campfire_random_bytes(value requested_length) {
  CAMLparam1(requested_length);
  CAMLlocal1(output);
  int length = Int_val(requested_length);
  if (length < 0 || length > 65536) caml_invalid_argument("invalid random length");
  output = caml_alloc_string(length);
  if (length > 0 && RAND_bytes((unsigned char *)Bytes_val(output), length) != 1) {
    fail_crypto();
  }
  CAMLreturn(output);
}

CAMLprim value campfire_aes_256_gcm_encrypt(value key, value nonce, value data) {
  CAMLparam3(key, nonce, data);
  CAMLlocal1(output);
  EVP_CIPHER_CTX *ctx = NULL;
  int written = 0, final_written = 0;
  mlsize_t data_len = caml_string_length(data);

  if (caml_string_length(key) != 32 || caml_string_length(nonce) != 12 ||
      data_len > INT_MAX) {
    caml_invalid_argument("invalid AES-256-GCM input");
  }
  output = caml_alloc_string(data_len + 16);
  ctx = EVP_CIPHER_CTX_new();
  if (ctx == NULL ||
      EVP_EncryptInit_ex(ctx, EVP_aes_256_gcm(), NULL, NULL, NULL) != 1 ||
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
    fail_crypto();
  }
  EVP_CIPHER_CTX_free(ctx);
  if ((mlsize_t)(written + final_written) != data_len) fail_crypto();
  CAMLreturn(output);
}

CAMLprim value campfire_aes_256_gcm_decrypt(value key, value nonce, value data) {
  CAMLparam3(key, nonce, data);
  CAMLlocal2(output, some);
  EVP_CIPHER_CTX *ctx = NULL;
  mlsize_t combined_len = caml_string_length(data);
  int written = 0, final_written = 0;
  mlsize_t cipher_len;

  if (caml_string_length(key) != 32 || caml_string_length(nonce) != 12 ||
      combined_len < 16 || combined_len - 16 > INT_MAX) {
    CAMLreturn(Val_none);
  }
  cipher_len = combined_len - 16;
  output = caml_alloc_string(cipher_len);
  ctx = EVP_CIPHER_CTX_new();
  if (ctx == NULL ||
      EVP_DecryptInit_ex(ctx, EVP_aes_256_gcm(), NULL, NULL, NULL) != 1 ||
      EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, 12, NULL) != 1 ||
      EVP_DecryptInit_ex(ctx, NULL, NULL,
                         (const unsigned char *)String_val(key),
                         (const unsigned char *)String_val(nonce)) != 1 ||
      EVP_DecryptUpdate(ctx, (unsigned char *)Bytes_val(output), &written,
                        (const unsigned char *)String_val(data),
                        (int)cipher_len) != 1 ||
      EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_TAG, 16,
                          (void *)(String_val(data) + cipher_len)) != 1) {
    EVP_CIPHER_CTX_free(ctx);
    CAMLreturn(Val_none);
  }
  int authenticated = EVP_DecryptFinal_ex(
                          ctx, (unsigned char *)Bytes_val(output) + written,
                          &final_written) == 1;
  EVP_CIPHER_CTX_free(ctx);
  if (!authenticated || (mlsize_t)(written + final_written) != cipher_len) {
    OPENSSL_cleanse(Bytes_val(output), cipher_len);
    CAMLreturn(Val_none);
  }
  some = caml_alloc(1, 0);
  Store_field(some, 0, output);
  CAMLreturn(some);
}
