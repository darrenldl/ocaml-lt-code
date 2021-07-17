#include <string.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

CAMLprim void xor_onto_stub (value len_, value src_, value onto_) {
  int len = Int_val(len_);
  const char* src = (const char*) Caml_ba_data_val(src_);
  char* onto = (char*) Caml_ba_data_val(onto_);
  for (int i = 0; i < len; i++) {
    onto[i] ^= src[i];
  }
}

CAMLprim void memcpy_stub (value dst_, value src_, value len_) {
  int len = Int_val(len_);
  const char* src = (const char*) Caml_ba_data_val(src_);
  char* dst = (char*) Caml_ba_data_val(dst_);
  memcpy(dst, src, len);
}
