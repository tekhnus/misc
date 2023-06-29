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
 {not "std" not}
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
   }
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

context-abort-reason := (/prelude/c-function selflib "context_abort_reason" {{'pointer}
  'string})

panic-if-aborted := fn {ctxt}
{reason := (/prelude/context-abort-reason ctxt)
 if (/std/not (/std/eq reason ""))
 {{} := (/std/panic @0 reason)}
 {}
 return ^{}}

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
 {} := (../panic-if-aborted @0 ctxt)
 return nothing}

compile-prog-new := fn {sl bpptr src compdata bdrcompdata ex}
{ctxt := (/prelude/context-make)
 e := (/prelude/prog-build-one-c-host-2 (/prelude/wrap-pointer-into-pointer sl) (/prelude/wrap-pointer-into-pointer bpptr) (/prelude/wrap-pointer-into-pointer src) compdata bdrcompdata (/prelude/get-host-ffi-settings) ex ctxt)
 {} := (../panic-if-aborted @0 ctxt)
 return {:ok
  :nothing}}

routine-run-and-get-value-c-host-new := (/prelude/c-function selflib "routine_run_in_ffi_host" {{'pointer
   'pointer
   'pointer}
  'pointer})

repr-pointer-impl := (/prelude/c-function selflib "datum_repr" {{'pointer
   }
  'string})

repr-pointer := fn {x} {return (/prelude/repr-pointer-impl x)}

eval-new := fn {sl rt0}
{ctxt := (/prelude/context-make)
 res := (/prelude/routine-run-and-get-value-c-host-new (/prelude/wrap-pointer-into-pointer sl) rt0 ctxt)
 {} := (../panic-if-aborted @0 ctxt)
 return {:ok
  res}}

read-one-ptr := (/prelude/dlsym selflib "datum_read_one")

datum-read-one := fn {x}
{return (/prelude/call-the-extension (/prelude/dereference read-one-ptr 'int64) x)}

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
 {panic-if-aborted panic-if-aborted}
 {ext-make ext-make}}
