#include "caml/mlvalues.h"
#include "caml/memory.h"

#include <stdlib.h>

value caml_realpath(value _path, value _resoved) {
  CAMLparam2(_path, _resolved);
  CAMLlocal1(_ans);
  const char* path = String_val(_path);
  char* res = String_val(_resolved);
  char* ans = realpath(path, resolved)
  _ans = Val_string(res);
  CAMLreturn(_ans);
}