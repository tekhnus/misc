(req
 (prelude "prelude")
 (shared-library "prelude" shared-library)
 (c-function-or-panic "prelude" c-function-or-panic)
 (extern-pointer "prelude" extern-pointer)
 (std "std")
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (repr "std" repr)
 (panic "std" panic)
 (first-good-value "std" first-good-value))

(def libc (std @slash first-good-value `(
  ~(prelude @slash shared-library "libc.so.6")
  ~(prelude @slash shared-library "libSystem.B.dylib"))))

(def malloc (prelude @slash c-function-or-panic libc "malloc" '((sizet) pointer)))
(def fopen (prelude @slash c-function-or-panic libc "fopen" '((string string) pointer)))
(def fread (prelude @slash c-function-or-panic libc "fread" '((pointer sizet sizet pointer) sizet)))
(def feof (prelude @slash c-function-or-panic libc "feof" '((pointer) int)))
(def fprintf (prelude @slash c-function-or-panic libc "fprintf" '((pointer string) sizet)))
(def fprintf-bytestring (prelude @slash c-function-or-panic libc "fprintf" '((pointer string string) sizet)))
(def printfptr (prelude @slash c-function-or-panic libc "printf" '((string pointer) sizet)))

(def stdin (std @slash first-good-value `(
  ~(prelude @slash extern-pointer libc "stdin" 'pointer)
  ~(prelude @slash extern-pointer libc "__stdinp" 'pointer))))

(def stdout (std @slash first-good-value `(
  ~(prelude @slash extern-pointer libc "stdout" 'pointer)
  ~(prelude @slash extern-pointer libc "__stdoutp" 'pointer))))

(def stderr (std @slash first-good-value `(
  ~(prelude @slash extern-pointer libc "stderr" 'pointer)
  ~(prelude @slash extern-pointer libc "__stderrp" 'pointer))))

(builtin.defun print (val)
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
