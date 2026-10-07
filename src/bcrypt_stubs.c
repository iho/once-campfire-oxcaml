#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <stdlib.h>
#include <string.h>

#include "vendor/passe-bcrypt/bcrypt.h"

CAMLprim value campfire_bcrypt_hashpass(value password, value salt) {
  CAMLparam2(password, salt);
  CAMLlocal1(result);
  char encrypted[BCRYPT_HASHSPACE];
  char salt_buffer[30];
  mlsize_t password_length = caml_string_length(password);
  char *password_buffer;
  int status;

  if (caml_string_length(salt) != 29) {
    caml_invalid_argument("bcrypt salt must be 29 bytes");
  }
  if (!caml_string_is_c_safe(password) || !caml_string_is_c_safe(salt)) {
    caml_invalid_argument("bcrypt values cannot contain null bytes");
  }

  password_buffer = malloc(password_length + 1);
  if (password_buffer == NULL) caml_failwith("bcrypt allocation failed");
  memcpy(password_buffer, String_val(password), password_length);
  password_buffer[password_length] = '\0';
  memcpy(salt_buffer, String_val(salt), 29);
  salt_buffer[29] = '\0';
  caml_enter_blocking_section();
  status = passe_bcrypt_hashpass(password_buffer, salt_buffer, encrypted,
                                 sizeof(encrypted));
  caml_leave_blocking_section();
  explicit_bzero(password_buffer, password_length + 1);
  free(password_buffer);
  if (status != 0) {
    caml_failwith("bcrypt verification failed");
  }
  result = caml_copy_string(encrypted);
  explicit_bzero(encrypted, sizeof(encrypted));
  explicit_bzero(salt_buffer, sizeof(salt_buffer));
  CAMLreturn(result);
}
