req
{{prelude "prelude"}
 {dlopen-or-error "prelude" dlopen-or-error}
 {c-function "prelude" c-function}
 {selflib "prelude" selflib}
 {null-pointer "prelude" null-pointer}
 {std "std"}
 {decons-pat "std" decons-pat}
 {first-good-value "std" first-good-value}
 {not "std" not}
 {eq "std" eq}
 {head "std" head}
 {cons "std" cons}
 {tail "std" tail}
 {panic "std" panic}}

buildlib := (/std/first-good-value {(/prelude/dlopen-or-error "libzlisp-build-lib.so")
  (/prelude/dlopen-or-error "libzlisp-build-lib.dylib")})

compdata-make := (/prelude/c-function selflib "compdata_alloc_make" {{}
  'pointer})

make-routine-with-empty-state := (/prelude/c-function selflib "routine_make_alloc" {{'sizet}
  'pointer})

prog-slice-make := (/prelude/c-function selflib "vec_alloc_slice" {{}
  'pointer})

prog-build-one-c-host-2 := (/prelude/c-function buildlib "prog_build" {{'pointer
   'pointer
   'pointer
   'pointer
   'pointer
   'pointer
   'pointer
   'pointer}
  'pointer})

context-make := (/prelude/c-function selflib "context_alloc_make" {{}
  'pointer})

context-abort-reason := (/prelude/c-function selflib "context_abort_reason" {{'pointer}
  'pointer})

panic-if-aborted := fn {ctxt}
{reason := (/prelude/context-abort-reason ctxt)
 if (/std/not (/std/eq reason null-pointer))
 {{} := (/std/panic @0 reason)}
 {}
 return ^{}}

prog-build-init := (/prelude/c-function buildlib "prog_build_init_alloc" {{'pointer
   'pointer
   'pointer
   'pointer}
  'pointer})

get-host-ffi-settings := (/prelude/c-function buildlib "get_host_ffi_settings" {{}
  'pointer})

ext-make := (/prelude/c-function buildlib "standard_extension_alloc_make" {{'pointer}
  'pointer})

init-prog := fn {sl compdata bdrcompdata}
{ctxt := (/prelude/context-make)
 bprog := (/prelude/prog-build-init sl compdata bdrcompdata ctxt)
 {} := (../panic-if-aborted @0 ctxt)
 return bprog}

compile-prog-new := fn {sl bpptr src compdata bdrcompdata ex}
{ctxt := (/prelude/context-make)
 e := (/prelude/prog-build-one-c-host-2 sl bpptr src compdata bdrcompdata (/prelude/get-host-ffi-settings) ex ctxt)
 {} := (../panic-if-aborted @0 ctxt)
 return {:ok
  :nothing}}

routine-run-and-get-value-c-host-new := (/prelude/c-function selflib "routine_run_in_ffi_host" {{'pointer
   'pointer
   'pointer}
  'pointer})

repr-pointer-impl := (/prelude/c-function selflib "datum_repr" {{'pointer}
  'pointer})

repr-pointer := fn {x}
{return (/prelude/repr-pointer-impl x)}

eval-new := fn {sl rt0}
{ctxt := (/prelude/context-make)
 res := (/prelude/routine-run-and-get-value-c-host-new sl rt0 ctxt)
 {} := (../panic-if-aborted @0 ctxt)
 return {:ok
  res}}

read-all-alloc := (/prelude/c-function selflib "datum_alloc_read_all" {{'pointer
   'pointer}
  'pointer})

datum-is-nil := (/prelude/c-function selflib "datum_is_nil" {{'pointer}
  'pointer})

read-new := fn {strm}
{ctxt := (/prelude/context-make)
 res := (/prelude/read-all-alloc strm ctxt)
 {} := (../panic-if-aborted @0 ctxt)
 is-eof := (/prelude/datum-is-nil res)
 if (/std/eq is-eof null-pointer)
 {return {:ok
   res}}
 {return {:eof}}}

export
{{compile-prog-new compile-prog-new}
 {init-prog init-prog}
 {eval-new eval-new}
 {read read-new}
 {repr-pointer repr-pointer}
 {make-routine-with-empty-state make-routine-with-empty-state}
 {prog-slice-make prog-slice-make}
 {compdata-make compdata-make}
 {context-make context-make}
 {panic-if-aborted panic-if-aborted}
 {ext-make ext-make}}
