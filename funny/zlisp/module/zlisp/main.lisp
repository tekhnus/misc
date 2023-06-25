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

prog-build-one-c-host-2 := (/prelude/c-function buildlib "prog_build" {{'pointer
   'pointer
   'pointer
   'pointer
   'pointer
   'pointer
   'pointer
   'pointer}
  'sizet})

context-make := (/prelude/c-function selflib "context_alloc_make" {{}
  'pointer})

exit-if-aborted := (/prelude/c-function selflib "exit_if_aborted" {{'pointer}
  'sizet})

prog-build-init := (/prelude/c-function buildlib "prog_build_init" {{'pointer
   'pointer
   'pointer
   'pointer}
  'sizet})

get-host-ffi-settings := (/prelude/c-function buildlib "get_host_ffi_settings" {{}
  'pointer})

ext-make := (/prelude/c-function buildlib "standard_extension_alloc_make" {{'pointer}
  'pointer})

init-prog := fn {sl compdata bdrcompdata}
{ctxt := (/prelude/context-make)
 nothing := (/prelude/prog-build-init (/prelude/wrap-pointer-into-pointer sl) compdata bdrcompdata ctxt)
 foo := (/prelude/exit-if-aborted ctxt)
 return nothing}

compile-prog-new := fn {sl bpptr src compdata bdrcompdata ex}
{ctxt := (/prelude/context-make)
 e := (/prelude/prog-build-one-c-host-2 (/prelude/wrap-pointer-into-pointer sl) (/prelude/wrap-pointer-into-pointer bpptr) (/prelude/wrap-pointer-into-pointer src) compdata bdrcompdata (/prelude/get-host-ffi-settings) ex ctxt)
 foo := (/prelude/exit-if-aborted ctxt)
 return {:ok
   :nothing}}

routine-run-and-get-value-c-host-new := (/prelude/c-function selflib "routine_run_in_ffi_host" {{'pointer
   'pointer 'pointer}
  'pointer})

repr-datum-pointer-ptr := (/prelude/dlsym selflib "repr_datum_pointer")

repr-pointer := fn {x}
{return (/prelude/call-the-extension (/prelude/dereference repr-datum-pointer-ptr 'int64) x)}

eval-new := fn {sl rt0}
{ctxt := (/prelude/context-make)
 res := (/prelude/routine-run-and-get-value-c-host-new (/prelude/wrap-pointer-into-pointer sl) rt0 ctxt)
 foo := (/prelude/exit-if-aborted ctxt)
 return {:ok res}}

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
 {context-make context-make}
 {exit-if-aborted exit-if-aborted}
 {ext-make ext-make}}
