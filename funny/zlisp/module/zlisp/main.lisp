req
{{prelude
  "prelude"}
 {shared-library
  "prelude"
  shared-library}
 {c-function
  "prelude"
  c-function}
 {selflib
  "prelude"
  selflib}
 {dlsym
  "prelude"
  dlsym}
 {dereference
  "prelude"
  dereference}
 {wrap-pointer-into-pointer
  "prelude"
  wrap-pointer-into-pointer}
 {call-extension-1
  "prelude"
  call-extension-1}
 {std
  "std"}
 {decons-pat
  "std"
  decons-pat}
 {first-good-value
  "std"
  first-good-value}
 {eq
  "std"
  eq}
 {head
  "std"
  head}
 {cons
  "std"
  cons}
 {tail
  "std"
  tail}
 {panic
  "std"
  panic}}

buildlib = (/std/first-good-value `(~(/prelude/shared-library "libzlisp-build-lib.so")))
compdata-make = (/prelude/c-function selflib "compdata_alloc_make" '(() pointer))
make-routine-with-empty-state = (/prelude/c-function selflib "routine_make_alloc" '((sizet pointer) pointer))
prog-slice-make = (/prelude/c-function selflib "vec_create_slice" '(() progslice))
prog-build-one-c-host = (/prelude/c-function buildlib "prog_build" '((pointer pointer pointer pointer pointer pointer pointer) pointer))
prog-build-init = (/prelude/c-function buildlib "prog_build_init" '((pointer pointer pointer) sizet))
get-host-ffi-settings = (/prelude/c-function buildlib "get_host_ffi_settings" '(() pointer))
ext-make = (/prelude/c-function buildlib "standard_extension_alloc_make" '(() pointer))
defn init-prog (sl compdata bdrcompdata)
{nothing = (/prelude/prog-build-init (/prelude/wrap-pointer-into-pointer sl) compdata bdrcompdata)
 return nothing}

defn compile-prog-new (sl bpptr src compdata bdrcompdata ex)
{e = (/prelude/prog-build-one-c-host (/prelude/wrap-pointer-into-pointer sl) (/prelude/wrap-pointer-into-pointer bpptr) (/prelude/wrap-pointer-into-pointer src) compdata bdrcompdata (/prelude/get-host-ffi-settings) ex)
 {if (/std/eq 0 (/prelude/dereference e 'int64))
  {return `(:ok :nothing)}
  {return `(:err ~(/prelude/dereference e 'string))}}}

routine-run-and-get-value-c-host-new = (/prelude/c-function selflib "routine_run_in_ffi_host" '((progslice pointer) fdatum))
fdatum-is-panic = (/prelude/c-function selflib "fdatum_is_panic" '((fdatum) int))
fdatum-get-value-ptr = (/prelude/dlsym selflib "fdatum_get_value")
defn fdatum-get-value (x)
{return (/prelude/call-extension-1 (/prelude/dereference fdatum-get-value-ptr 'int64) x)}

fdatum-get-panic-message-ptr = (/prelude/dlsym selflib "fdatum_get_panic_message")
defn fdatum-get-panic-message (x)
{return (/prelude/call-extension-1 (/prelude/dereference fdatum-get-panic-message-ptr 'int64) x)}

fdatum-repr-datum-pointer-ptr = (/prelude/dlsym selflib "fdatum_repr_datum_pointer")
defn repr-pointer (x)
{return (/prelude/call-extension-1 (/prelude/dereference fdatum-repr-datum-pointer-ptr 'int64) x)}

defn eval-new (sl rt0)
{res = (/prelude/routine-run-and-get-value-c-host-new sl rt0)
 {if (/std/eq (/prelude/fdatum-is-panic res) 1)
  {msg = (../fdatum-get-panic-message res)
   {return `(:err ~msg)}}
  {val = (../fdatum-get-value res)
   {return `(:ok ~val ~rt0)}}}}

datum-read-one = (/prelude/c-function selflib "datum_read_one" '((pointer) fdatum))
defn read (strm)
{res = (/prelude/datum-read-one strm)
 {if (/std/eq (/prelude/fdatum-is-panic res) 1)
  {msg = (../fdatum-get-panic-message res)
   {if (/std/eq msg "eof")
    {return '(:eof)}
    {return `(:err ~msg)}}}
  {maybeval = (../fdatum-get-value res)
   {return `(:ok ~maybeval)}}}}

export
{(compile-prog-new compile-prog-new)
 (init-prog init-prog)
 (eval-new eval-new)
 (read read)
 (repr-pointer repr-pointer)
 (make-routine-with-empty-state make-routine-with-empty-state)
 (prog-slice-make prog-slice-make)
 (compdata-make compdata-make)
 (ext-make ext-make)}
