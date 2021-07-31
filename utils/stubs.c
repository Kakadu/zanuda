#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"

#include <stdlib.h>
#include <assert.h>

value caml_realpath(value _path) {
  CAMLparam1(_path);
  CAMLlocal1(_ans);
  const char* path = String_val(_path);
  char* ans = realpath(path, NULL);
  if (ans) {
    _ans = caml_copy_string(ans);
    free(ans);
    CAMLreturn(_ans);
  } else 
    assert(0);
}