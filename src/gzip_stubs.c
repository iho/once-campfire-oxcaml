#include <zlib.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <limits.h>
#include <string.h>

CAMLprim value caml_gzip_compress(value input) {
  CAMLparam1(input);
  CAMLlocal2(buffer, output);

  mlsize_t input_length = caml_string_length(input);
  if (input_length > UINT_MAX) {
    caml_invalid_argument("gzip input is too large");
  }

  z_stream stream;
  memset(&stream, 0, sizeof(stream));
  int result = deflateInit2(&stream, Z_DEFAULT_COMPRESSION, Z_DEFLATED,
                            MAX_WBITS + 16, 8, Z_DEFAULT_STRATEGY);
  if (result != Z_OK) caml_failwith("gzip initialization failed");

  uLong bound = deflateBound(&stream, (uLong)input_length);
  if (bound > UINT_MAX) {
    deflateEnd(&stream);
    caml_invalid_argument("gzip output is too large");
  }

  buffer = caml_alloc_string((mlsize_t)bound);
  stream.next_in = (Bytef *)String_val(input);
  stream.avail_in = (uInt)input_length;
  stream.next_out = (Bytef *)Bytes_val(buffer);
  stream.avail_out = (uInt)bound;

  result = deflate(&stream, Z_FINISH);
  uLong output_length = stream.total_out;
  deflateEnd(&stream);
  if (result != Z_STREAM_END) caml_failwith("gzip compression failed");

  output = caml_alloc_string((mlsize_t)output_length);
  memcpy(Bytes_val(output), Bytes_val(buffer), (size_t)output_length);
  CAMLreturn(output);
}
