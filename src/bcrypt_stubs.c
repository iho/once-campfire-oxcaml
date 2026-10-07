#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <string.h>

#include "vendor/passe-bcrypt/bcrypt.h"

CAMLprim value campfire_bcrypt_hashpass(value password, value salt) {
  CAMLparam2(password, salt);
  CAMLlocal1(result);
  char encrypted[BCRYPT_HASHSPACE];
  char salt_buffer[30];

  if (caml_string_length(salt) != 29) {
    caml_invalid_argument("bcrypt salt must be 29 bytes");
  }
  if (!caml_string_is_c_safe(password) || !caml_string_is_c_safe(salt)) {
    caml_invalid_argument("bcrypt values cannot contain null bytes");
  }

  memcpy(salt_buffer, String_val(salt), 29);
  salt_buffer[29] = '\0';
  if (passe_bcrypt_hashpass(String_val(password), salt_buffer, encrypted,
                            sizeof(encrypted)) != 0) {
    caml_failwith("bcrypt verification failed");
  }
  result = caml_copy_string(encrypted);
  explicit_bzero(encrypted, sizeof(encrypted));
  explicit_bzero(salt_buffer, sizeof(salt_buffer));
  CAMLreturn(result);
}
