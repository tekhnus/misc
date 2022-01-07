!(require "stdmacro")
(require "std")

!(#def-or-panica libc
  (shared-library "libc.so.6")
  (shared-library "libSystem.B.dylib"))

!(#def-or-panica malloc-
     (extern-pointer libc "malloc"
		     '((sizet) pointer)))
!(#wrap-fn-pointer malloc malloc-)

!(#def-or-panica fopen-
     (extern-pointer libc "fopen"
		     '((string string) pointer)))
!(#wrap-fn-pointer fopen fopen-)

!(#def-or-panica fread-
     (extern-pointer libc "fread"
		     '((pointer sizet sizet pointer) sizet)))
!(#wrap-fn-pointer fread fread-)

!(#def-or-panica feof-
     (extern-pointer libc "feof"
		     '((pointer) int)))
!(#wrap-fn-pointer feof feof-)

!(#def-or-panica fprintf-
     (extern-pointer libc "fprintf"
		     '((pointer string) sizet)))
!(#wrap-fn-pointer fprintf fprintf-)

!(#def-or-panica fprintf-bytestring-
     (extern-pointer libc "fprintf"
		     '((pointer string string) sizet)))
!(#wrap-fn-pointer fprintf-bytestring fprintf-bytestring-)

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
