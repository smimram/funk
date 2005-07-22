#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

#include "setup.h"
#include "mm.h"

CAMLprim value caml_funk_mem_size(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_int(mem_size));
}

CAMLprim value caml_funk_remap_pr(value pgd, value phys_addr, value len, value virt_addr, value attr)
{
  CAMLparam5(pgd, phys_addr, len, virt_addr, attr);
  map_page_range((unsigned long*)Nativeint_val(pgd), Nativeint_val(phys_addr), Nativeint_val(len), Nativeint_val(virt_addr), Int_val(attr));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_funk_get_mem(value addr)
{
  CAMLparam1(addr);
  CAMLreturn(Int_val(addr));
}

CAMLprim value caml_funk_malloc(value len)
{
  CAMLparam1(len);
  CAMLreturn((int)malloc(Int_val(len)));
}
