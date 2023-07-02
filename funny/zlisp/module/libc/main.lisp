req
{{prelude "prelude"}
 {dlopen-or-error "prelude" dlopen-or-error}
 {c-function "prelude" c-function}
 {dlsym-or-error "prelude" dlsym-or-error}
 {deref "prelude" deref}
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
  'int64})

fread := (/prelude/c-function libc "fread" {{'pointer
   'sizet
   'sizet
   'int64}
  'sizet})

feof := (/prelude/c-function libc "feof" {{'int64}
  'int})

fprintf := (/prelude/c-function libc "fprintf" {{'int64
   'string}
  'sizet})

fprintf-new := (/prelude/c-function libc "fprintf" {{'int64
   'string}
  'sizet})

fprintf-pointer := (/prelude/c-function libc "fprintf" {{'int64
   'pointer}
  'sizet})
fprintf-pointer-new := (/prelude/c-function libc "fprintf" {{'int64
   'pointer}
  'sizet})

stdin := (/std/first-good-value {(/prelude/dlsym-or-error libc "stdin")
  (/prelude/dlsym-or-error libc "__stdinp")})
stdin-val := (/prelude/deref stdin 'int64)

stdout := (/std/first-good-value {(/prelude/dlsym-or-error libc "stdout")
  (/prelude/dlsym-or-error libc "__stdoutp")})
stdout-val := (/prelude/deref stdout 'int64)

stderr := (/std/first-good-value {(/prelude/dlsym-or-error libc "stderr")
  (/prelude/dlsym-or-error libc "__stderrp")})
stderr-val := (/prelude/deref stderr 'int64)

print := fn {val}
{return (/prelude/fprintf-new stdout-val (/std/repr val))}

export
{{malloc malloc}
 {fopen fopen}
 {fread fread}
 {fprintf-new fprintf-new}
 {fprintf-pointer-new fprintf-pointer-new}
 {stdin-val stdin-val}
 {stdout-val stdout-val}
 {stderr-val stderr-val}
 {print print}}
