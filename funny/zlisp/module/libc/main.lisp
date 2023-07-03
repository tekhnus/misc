req
{{prelude "prelude"}
 {dlopen-or-error "prelude" dlopen-or-error}
 {c-function "prelude" c-function}
 {dlsym-or-error "prelude" dlsym-or-error}
 {deref "prelude" deref}
 {ser "prelude" ser}
 {copy-to-heap "prelude" copy-to-heap}
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

fopen-impl := (/prelude/c-function libc "fopen" {{'pointer
   'pointer}
  'pointer})

fopen := fn {name mode} {
  name-ser := (/prelude/ser name)
  name-on-heap := (/prelude/copy-to-heap name-ser)
  mode-ser := (/prelude/ser mode)
  mode-on-heap := (/prelude/copy-to-heap mode-ser)
  return (/prelude/fopen-impl name-on-heap mode-on-heap)
}

fread := (/prelude/c-function libc "fread" {{'pointer
   'sizet
   'sizet
   'pointer}
  'sizet})

fprintf-pointer-new := (/prelude/c-function libc "fprintf" {{'pointer
   'pointer}
  'sizet})

fprintf-new := fn {x y} {
y-ser := (/prelude/ser y)
y-on-heap := (/prelude/copy-to-heap y-ser)
return (/prelude/fprintf-pointer-new x y-on-heap)}

stdin := (/std/first-good-value {(/prelude/dlsym-or-error libc "stdin")
  (/prelude/dlsym-or-error libc "__stdinp")})
stdin-val := (/prelude/deref stdin 'pointer)

stdout := (/std/first-good-value {(/prelude/dlsym-or-error libc "stdout")
  (/prelude/dlsym-or-error libc "__stdoutp")})
stdout-val := (/prelude/deref stdout 'pointer)

stderr := (/std/first-good-value {(/prelude/dlsym-or-error libc "stderr")
  (/prelude/dlsym-or-error libc "__stderrp")})
stderr-val := (/prelude/deref stderr 'pointer)

print := fn {val}
{return (../fprintf-new stdout-val (/std/repr val))}

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
