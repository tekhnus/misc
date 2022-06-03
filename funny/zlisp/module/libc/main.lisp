(req
 (shared-library "prelude" shared-library)
 (c-function-or-panic "prelude" c-function-or-panic)
 (extern-pointer "prelude" extern-pointer)
 (std "std"))
(importall std)

!(req (stdmacro "stdmacro"))
!(importall stdmacro)

!(#def-or-panica libc
  (shared-library "libc.so.6")
  (shared-library "libSystem.B.dylib"))

(def malloc (c-function-or-panic libc "malloc" '((sizet) pointer)))
(def fopen (c-function-or-panic libc "fopen" '((string string) pointer)))
(def fread (c-function-or-panic libc "fread" '((pointer sizet sizet pointer) sizet)))
(def feof (c-function-or-panic libc "feof" '((pointer) int)))
(def fprintf (c-function-or-panic libc "fprintf" '((pointer string) sizet)))
(def fprintf-bytestring (c-function-or-panic libc "fprintf" '((pointer string string) sizet)))
(def printfptr (c-function-or-panic libc "printf" '((string pointer) sizet)))

!(#def-or-panica stdin
  (extern-pointer libc "stdin" 'pointer)
  (extern-pointer libc "__stdinp" 'pointer))

!(#def-or-panica stdout
  (extern-pointer libc "stdout" 'pointer)
  (extern-pointer libc "__stdoutp" 'pointer))

!(#def-or-panica stderr
  (extern-pointer libc "stderr" 'pointer)
  (extern-pointer libc "__stderrp" 'pointer))

!(#defun print (val)
  (return (fprintf-bytestring stdout "%s\n" (repr val))))

(export
 (malloc malloc)
 (fopen fopen)
 (fread fread)
 (feof feof)
 (fprintf fprintf)
 (fprintf-bytestring fprintf-bytestring)
 (printfptr printfptr)
 (stdin stdin)
 (stdout stdout)
 (stderr stderr)
 (print print))
