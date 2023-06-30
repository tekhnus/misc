req
{{prelude "prelude"}
 {dlopen-or-error "prelude" dlopen-or-error}
 {c-function "prelude" c-function}
 {dlsym-or-error "prelude" dlsym-or-error}
 {dlsym-or-error2 "prelude" dlsym-or-error2}
 {std "std"}
 {decons-pat "std" decons-pat}
 {eq "std" eq}
 {head "std" head}
 {cons "std" cons}
 {repr "std" repr}
 {panic "std" panic}
 {first-good-value "std" first-good-value}}

libc := (/std/first-good-value {(/prelude/dlopen-or-error "libc.so.6")
  (/prelude/dlopen-or-error "libSystem.B.dylib")})

malloc := (/prelude/c-function libc "malloc" {{'sizet}
  'pointer})

fopen := (/prelude/c-function libc "fopen" {{'string
   'string}
  'pointer})

fread := (/prelude/c-function libc "fread" {{'pointer
   'sizet
   'sizet
   'pointer}
  'sizet})

feof := (/prelude/c-function libc "feof" {{'pointer}
  'int})

fprintf := (/prelude/c-function libc "fprintf" {{'pointer
   'string}
  'sizet})

fprintf-pointer := (/prelude/c-function libc "fprintf" {{'pointer
   'pointer}
  'sizet})

stdin := (/std/first-good-value {(/prelude/dlsym-or-error2 libc "stdin" )
  (/prelude/dlsym-or-error2 libc "__stdinp" )})

stdout := (/std/first-good-value {(/prelude/dlsym-or-error2 libc "stdout")
  (/prelude/dlsym-or-error2 libc "__stdoutp")})

stderr := (/std/first-good-value {(/prelude/dlsym-or-error2 libc "stderr")
  (/prelude/dlsym-or-error2 libc "__stderrp")})

print := fn {val}
{return (/prelude/fprintf stdout (/std/repr val))}

export
{{malloc malloc}
 {fopen fopen}
 {fread fread}
 {feof feof}
 {fprintf fprintf}
 {fprintf-pointer fprintf-pointer}
 {stdin stdin}
 {stdout stdout}
 {stderr stderr}
 {print print}}
