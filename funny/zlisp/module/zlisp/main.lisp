req
{{prelude "prelude"}
 {dlopen-or-error "prelude" dlopen-or-error}
 {c-function "prelude" c-function}
 {selflib "prelude" selflib}
 {ser "prelude" ser}
 {null-pointer "prelude" null-pointer}
 {malloc-and-copy "prelude" malloc-and-copy}
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

compdata-make-impl := (/prelude/c-function selflib "compdata_make" {{}
  'datum})
compdata-make := fn {} {
c := (/prelude/compdata-make-impl)
return (/prelude/malloc-and-copy c)
}

make-routine-with-empty-state-impl := (/prelude/c-function selflib "routine_make_topmost" {{'int64_t}
  'datum})
make-routine-with-empty-state := fn {} {
r := (/prelude/make-routine-with-empty-state-impl (/prelude/ser 0))
return (/prelude/malloc-and-copy r)}

prog-slice-make-impl := (/prelude/c-function selflib "vec_make" {{'sizet}
  'vec})
prog-slice-make := fn {} {
zero := (/prelude/ser 0)
v := (/prelude/prog-slice-make-impl zero)
return (/prelude/malloc-and-copy v)}

prog-build-one-c-host-2 := (/prelude/c-function buildlib "prog_build" {{'pointer
   'pointer
   'pointer
   'pointer
   'pointer
   'datum
   'pointer
   'pointer}
  'pointer})

context-make-impl-2 := (/prelude/c-function selflib "context_make" {{}
  'context})
context-make := fn {} {
ctxt := (/prelude/context-make-impl-2)
ptr := (/prelude/malloc-and-copy ctxt)
return ptr}

context-abort-reason := (/prelude/c-function selflib "context_abort_reason" {{'pointer}
  'pointer})

panic-if-aborted := fn {ctxt}
{reason := (/prelude/context-abort-reason ctxt)
 if (/std/not (/std/eq reason null-pointer))
 {{} := (/std/panic @0 reason)}
 {}
 return ^{}}

prog-build-init := (/prelude/c-function buildlib "prog_build_init" {{'pointer
   'pointer
   'pointer
   'pointer}
  'sizet})

get-host-ffi-settings := (/prelude/c-function buildlib "get_host_ffi_settings" {{}
  'datum})

ext-make-impl := (/prelude/c-function buildlib "standard_extension_make" {{'pointer}
  'lisp_extension})
ext-make := fn {x} {
e := (/prelude/ext-make-impl x)
return (/prelude/malloc-and-copy e)}

init-prog := fn {sl compdata bdrcompdata}
{ctxt := (../context-make)
 bprog := (/prelude/prog-build-init sl compdata bdrcompdata ctxt)
 {} := (../panic-if-aborted @0 ctxt)
 return (/prelude/malloc-and-copy bprog)}

compile-prog-new := fn {sl bpptr src compdata bdrcompdata ex}
{ctxt := (../context-make)
 e := (/prelude/prog-build-one-c-host-2 sl bpptr src compdata bdrcompdata (/prelude/get-host-ffi-settings) ex ctxt)
 {} := (../panic-if-aborted @0 ctxt)
 return {:ok
  :nothing}}

routine-run-and-get-value-c-host-new := (/prelude/c-function selflib "host_ffi_run" {{'pointer
   'pointer
   'datum
   'pointer}
  'result})

repr-pointer-impl := (/prelude/c-function selflib "datum_repr" {{'pointer}
  'pointer})

repr-pointer := fn {x}
{return (/prelude/repr-pointer-impl x)}

datum-make-nil := (/prelude/c-function selflib "datum_make_nil" {{}
  'datum})

result-get-value := (/prelude/c-function selflib "result_get_value" {{'pointer 'pointer}
  'pointer})

eval-new := fn {sl rt0}
{ctxt := (../context-make)
 ar := (/prelude/datum-make-nil)
 res := (/prelude/routine-run-and-get-value-c-host-new sl rt0 ar ctxt)
 {} := (../panic-if-aborted @0 ctxt)
 res-ptr := (/prelude/malloc-and-copy res)
 val := (/prelude/result-get-value res-ptr ctxt)
 {} := (../panic-if-aborted @0 ctxt)
 return {:ok
  val}}

read-all-alloc := (/prelude/c-function selflib "datum_read_all" {{'pointer
   'pointer}
  'vec})

datum-is-nil := (/prelude/c-function selflib "datum_is_nil" {{'pointer}
  'pointer})

datum-make-list := (/prelude/c-function selflib "datum_make_list" {{'vec}
  'datum})

read-new := fn {strm}
{ctxt := (../context-make)
 resvec := (/prelude/read-all-alloc strm ctxt)
 {} := (../panic-if-aborted @0 ctxt)
 resdat := (/prelude/datum-make-list resvec)
 res := (/prelude/malloc-and-copy resdat)
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
