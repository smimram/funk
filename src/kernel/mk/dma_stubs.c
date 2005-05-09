#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include "libc-dummy.h"

CAMLprim value caml_funk_dma_get_buffer(value vlen)
{
  CAMLparam1(vlen);
  char* buf = malloc(Int_val(vlen));
  CAMLreturn(caml_copy_nativeint((int)buf));
}

CAMLprim value caml_funk_dma_string_of_buffer(value vbuf, value vlen, value vdst)
{
  CAMLparam3(vbuf, vlen, vdst);
  int len = Int_val(vlen);
  memmove(String_val(vdst), (char*)Nativeint_val(vbuf), len);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_funk_dma_free_buffer(value vbuf)
{
  CAMLparam1(vbuf);
  free((char*)Nativeint_val(vbuf));
  CAMLreturn(Val_unit);
}
