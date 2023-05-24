req
{{prelude "prelude"}
 {shared-library "prelude" shared-library}
 {c-function "prelude" c-function}
 {extern-pointer "prelude" extern-pointer}
 {std "std"}
 {decons-pat "std" decons-pat}
 {eq "std" eq}
 {head "std" head}
 {cons "std" cons}
 {repr "std" repr}
 {panic "std" panic}
 {first-good-value "std" first-good-value}}

libc = {call {/std/first-good-value {list {{call {/prelude/shared-library "libc.so.6"}} {call {/prelude/shared-library "libSystem.B.dylib"}}}}}}
malloc = {call {/prelude/c-function libc "malloc" {list {{list {'sizet}} 'pointer}}}}
fopen = {call {/prelude/c-function libc "fopen" {list {{list {'string 'string}} 'pointer}}}}
fread = {call {/prelude/c-function libc "fread" {list {{list {'pointer 'sizet 'sizet 'pointer}} 'sizet}}}}
feof = {call {/prelude/c-function libc "feof" {list {{list {'pointer}} 'int}}}}
fprintf = {call {/prelude/c-function libc "fprintf" {list {{list {'pointer 'string}} 'sizet}}}}
fprintf-bytestring = {call {/prelude/c-function libc "fprintf" {list {{list {'pointer 'string 'string}} 'sizet}}}}
printfptr = {call {/prelude/c-function libc "printf" {list {{list {'string 'pointer}} 'sizet}}}}
stdin = {call {/std/first-good-value {list {{call {/prelude/extern-pointer libc "stdin" 'pointer}} {call {/prelude/extern-pointer libc "__stdinp" 'pointer}}}}}}
stdout = {call {/std/first-good-value {list {{call {/prelude/extern-pointer libc "stdout" 'pointer}} {call {/prelude/extern-pointer libc "__stdoutp" 'pointer}}}}}}
stderr = {call {/std/first-good-value {list {{call {/prelude/extern-pointer libc "stderr" 'pointer}} {call {/prelude/extern-pointer libc "__stderrp" 'pointer}}}}}}
defn print {val}
{return {call {/prelude/fprintf-bytestring stdout "%s\n" {call {/std/repr val}}}}}

export
{{malloc malloc}
 {fopen fopen}
 {fread fread}
 {feof feof}
 {fprintf fprintf}
 {fprintf-bytestring fprintf-bytestring}
 {printfptr printfptr}
 {stdin stdin}
 {stdout stdout}
 {stderr stderr}
 {print print}}
