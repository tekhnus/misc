req
{{prelude "prelude"}
 {shared-library "prelude" shared-library}
 {c-function "prelude" c-function}
 {selflib "prelude" selflib}
 {dlsym "prelude" dlsym}
 {dereference "prelude" dereference}
 {wrap-pointer-into-pointer "prelude" wrap-pointer-into-pointer}
 {call-the-extension "prelude" call-the-extension}
 {std "std"}
 {decons-pat "std" decons-pat}
 {first-good-value "std" first-good-value}
 {eq "std" eq}
 {head "std" head}
 {cons "std" cons}
 {tail "std" tail}
 {panic "std" panic}}

buildlib := (/std/first-good-value {(/prelude/shared-library "libzlisp-build-lib.so")
  (/prelude/shared-library "libzlisp-build-lib.dylib")})

compdata-make := (/prelude/c-function selflib "compdata_alloc_make" {{}
  'pointer})

make-routine-with-empty-state := (/prelude/c-function selflib "routine_make_alloc" {{'sizet
   'pointer}
  'pointer})

prog-slice-make := (/prelude/c-function selflib "vec_create_slice" {{}
  'progslice})

prog-build-one-c-host := (/prelude/c-function buildlib "prog_build_or_exit" {{'pointer
   'pointer
   'pointer
   'pointer
   'pointer
   'pointer
   'pointer}
  'pointer})

context-make := (/prelude/c-function selflib "context_alloc_make" {{}
  'pointer})

prog-build-init := (/prelude/c-function buildlib "prog_build_init" {{'pointer
   'pointer
   'pointer
   'pointer}
  'sizet})

get-host-ffi-settings := (/prelude/c-function buildlib "get_host_ffi_settings" {{}
  'pointer})

ext-make := (/prelude/c-function buildlib "standard_extension_alloc_make" {{}
  'pointer})

init-prog := fn {sl compdata bdrcompdata}
{ctxt := (/prelude/context-make)
 nothing := (/prelude/prog-build-init (/prelude/wrap-pointer-into-pointer sl) compdata bdrcompdata (/prelude/wrap-pointer-into-pointer ctxt))
 return nothing}

compile-prog-new := fn {sl bpptr src compdata bdrcompdata ex}
{e := (/prelude/prog-build-one-c-host (/prelude/wrap-pointer-into-pointer sl) (/prelude/wrap-pointer-into-pointer bpptr) (/prelude/wrap-pointer-into-pointer src) compdata bdrcompdata (/prelude/get-host-ffi-settings) ex)
 if (/std/eq 0 (/prelude/dereference e 'int64))
 {return {:ok
   :nothing}}
 {return {:err
   (/prelude/dereference e 'string)}}}

routine-run-and-get-value-c-host-new := (/prelude/c-function selflib "routine_run_in_ffi_host" {{'progslice
   'pointer}
  'fdatum})

fdatum-is-panic := (/prelude/c-function selflib "fdatum_is_panic" {{'fdatum}
  'int})

fdatum-get-value-ptr := (/prelude/dlsym selflib "fdatum_get_value")

fdatum-get-value := fn {x}
{return (/prelude/call-the-extension (/prelude/dereference fdatum-get-value-ptr 'int64) x)}

fdatum-get-panic-message-ptr := (/prelude/dlsym selflib "fdatum_get_panic_message")

fdatum-get-panic-message := fn {x}
{return (/prelude/call-the-extension (/prelude/dereference fdatum-get-panic-message-ptr 'int64) x)}

fdatum-repr-datum-pointer-ptr := (/prelude/dlsym selflib "fdatum_repr_datum_pointer")

repr-pointer := fn {x}
{return (/prelude/call-the-extension (/prelude/dereference fdatum-repr-datum-pointer-ptr 'int64) x)}

eval-new := fn {sl rt0}
{res := (/prelude/routine-run-and-get-value-c-host-new sl rt0)
 msg := 42
 val := 42
 if (/std/eq (/prelude/fdatum-is-panic res) 1)
 {msg = (../fdatum-get-panic-message res)
  return {:err
   msg}}
 {val = (../fdatum-get-value res)
  return {:ok
   val}}}

read-one-ptr := (/prelude/dlsym selflib "datum_read_one")

datum-read-one := fn {x}
{return (/prelude/call-the-extension
 (/prelude/dereference read-one-ptr 'int64) x)}

read := fn {strm}
{res := (../datum-read-one strm)
 return res}

export
{{compile-prog-new compile-prog-new}
 {init-prog init-prog}
 {eval-new eval-new}
 {read read}
 {repr-pointer repr-pointer}
 {make-routine-with-empty-state make-routine-with-empty-state}
 {prog-slice-make prog-slice-make}
 {compdata-make compdata-make}
 {ext-make ext-make}}
