req
{{prelude "prelude"}
 {shared-library "prelude" shared-library}
 {c-function "prelude" c-function}
 {selflib "prelude" selflib}
 {dlsym "prelude" dlsym}
 {dereference "prelude" dereference}
 {wrap-pointer-into-pointer "prelude" wrap-pointer-into-pointer}
 {call-extension-1 "prelude" call-extension-1}
 {std "std"}
 {decons-pat "std" decons-pat}
 {first-good-value "std" first-good-value}
 {eq "std" eq}
 {head "std" head}
 {cons "std" cons}
 {tail "std" tail}
 {panic "std" panic}}

buildlib = {call {/std/first-good-value {list {call {/prelude/shared-library "libzlisp-build-lib.so"}}}}}
compdata-make = {call {/prelude/c-function selflib "compdata_alloc_make" {list {{list {}} 'pointer}}}}
make-routine-with-empty-state = {call {/prelude/c-function selflib "routine_make_alloc" {list {{list {'sizet 'pointer}} 'pointer}}}}
prog-slice-make = {call {/prelude/c-function selflib "vec_create_slice" {list {{list {}} 'progslice}}}}
prog-build-one-c-host = {call {/prelude/c-function buildlib "prog_build" {list {{list {'pointer 'pointer 'pointer 'pointer 'pointer 'pointer 'pointer}} 'pointer}}}}
prog-build-init = {call {/prelude/c-function buildlib "prog_build_init" {list {{list {'pointer 'pointer 'pointer}} 'sizet}}}}
get-host-ffi-settings = {call {/prelude/c-function buildlib "get_host_ffi_settings" {list {{list {}} 'pointer}}}}
ext-make = {call {/prelude/c-function buildlib "standard_extension_alloc_make" {list {{list {}} 'pointer}}}}
defn init-prog {sl compdata bdrcompdata}
{nothing = {call {/prelude/prog-build-init {call {/prelude/wrap-pointer-into-pointer sl}} compdata bdrcompdata}}
 return nothing}

defn compile-prog-new {sl bpptr src compdata bdrcompdata ex}
{e = {call {/prelude/prog-build-one-c-host {call {/prelude/wrap-pointer-into-pointer sl}} {call {/prelude/wrap-pointer-into-pointer bpptr}} {call {/prelude/wrap-pointer-into-pointer src}} compdata bdrcompdata {call {/prelude/get-host-ffi-settings}} ex}}
 {if {call {/std/eq 0 {call {/prelude/dereference e 'int64}}}}
  {return {list {:ok :nothing}}}
  {return {list {:err {call {/prelude/dereference e 'string}}}}}}}

routine-run-and-get-value-c-host-new = {call {/prelude/c-function selflib "routine_run_in_ffi_host" {list {{list {'progslice 'pointer}} 'fdatum}}}}
fdatum-is-panic = {call {/prelude/c-function selflib "fdatum_is_panic" {list {{list {'fdatum}} 'int}}}}
fdatum-get-value-ptr = {call {/prelude/dlsym selflib "fdatum_get_value"}}
defn fdatum-get-value {x}
{return {call {/prelude/call-extension-1 {call {/prelude/dereference fdatum-get-value-ptr 'int64}} x}}}

fdatum-get-panic-message-ptr = {call {/prelude/dlsym selflib "fdatum_get_panic_message"}}
defn fdatum-get-panic-message {x}
{return {call {/prelude/call-extension-1 {call {/prelude/dereference fdatum-get-panic-message-ptr 'int64}} x}}}

fdatum-repr-datum-pointer-ptr = {call {/prelude/dlsym selflib "fdatum_repr_datum_pointer"}}
defn repr-pointer {x}
{return {call {/prelude/call-extension-1 {call {/prelude/dereference fdatum-repr-datum-pointer-ptr 'int64}} x}}}

defn eval-new {sl rt0}
{res = {call {/prelude/routine-run-and-get-value-c-host-new sl rt0}}
 {if {call {/std/eq {call {/prelude/fdatum-is-panic res}} 1}}
  {msg = {call {../fdatum-get-panic-message res}}
   {return {list {:err msg}}}}
  {val = {call {../fdatum-get-value res}}
   {return {list {:ok val rt0}}}}}}

datum-read-one = {call {/prelude/c-function selflib "datum_read_one" {list {{list {'pointer}} 'fdatum}}}}
defn read {strm}
{res = {call {/prelude/datum-read-one strm}}
 {if {call {/std/eq {call {/prelude/fdatum-is-panic res}} 1}}
  {msg = {call {../fdatum-get-panic-message res}}
   {if {call {/std/eq msg "eof"}}
    {return {list {':eof}}}
    {return {list {:err msg}}}}}
  {maybeval = {call {../fdatum-get-value res}}
   {return {list {:ok maybeval}}}}}}

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
