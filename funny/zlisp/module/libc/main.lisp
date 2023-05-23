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

libc = (/std/first-good-value `(~(/prelude/shared-library "libc.so.6") ~(/prelude/shared-library "libSystem.B.dylib")))
malloc = (/prelude/c-function libc "malloc" {list {quote {(sizet) pointer}}})
fopen = (/prelude/c-function libc "fopen" {list {quote {(string string) pointer}}})
fread = (/prelude/c-function libc "fread" {list {quote {(pointer sizet sizet pointer) sizet}}})
feof = (/prelude/c-function libc "feof" {list {quote {(pointer) int}}})
fprintf = (/prelude/c-function libc "fprintf" {list {quote {(pointer string) sizet}}})
fprintf-bytestring = (/prelude/c-function libc "fprintf" {list {quote {(pointer string string) sizet}}})
printfptr = (/prelude/c-function libc "printf" {list {quote {(string pointer) sizet}}})
stdin = (/std/first-good-value `(~(/prelude/extern-pointer libc "stdin" {quote {pointer}}) ~(/prelude/extern-pointer libc "__stdinp" {quote {pointer}})))
stdout = (/std/first-good-value `(~(/prelude/extern-pointer libc "stdout" {quote {pointer}}) ~(/prelude/extern-pointer libc "__stdoutp" {quote {pointer}})))
stderr = (/std/first-good-value `(~(/prelude/extern-pointer libc "stderr" {quote {pointer}}) ~(/prelude/extern-pointer libc "__stderrp" {quote {pointer}})))
defn print {val}
{return (/prelude/fprintf-bytestring stdout "%s\n" (/std/repr val))}

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
