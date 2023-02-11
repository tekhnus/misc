(req
 (prelude "prelude")
 (shared-library "prelude" shared-library)
 (c-function "prelude" c-function)
 (extern-pointer "prelude" extern-pointer)
 (std "std")
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (repr "std" repr)
 (panic "std" panic)
 (first-good-value "std" first-good-value))

(def libc (/std/first-good-value `(
                                  ~(/prelude/shared-library "libc.so.6")
                                  ~(/prelude/shared-library "libSystem.B.dylib"))))

(def malloc (/prelude/c-function libc "malloc" '((sizet) pointer)))
(def fopen (/prelude/c-function libc "fopen" '((string string) pointer)))
(def fread (/prelude/c-function libc "fread" '((pointer sizet sizet pointer) sizet)))
(def feof (/prelude/c-function libc "feof" '((pointer) int)))
(def fprintf (/prelude/c-function libc "fprintf" '((pointer string) sizet)))
(def fprintf-bytestring (/prelude/c-function libc "fprintf" '((pointer string string) sizet)))
(def printfptr (/prelude/c-function libc "printf" '((string pointer) sizet)))

(def stdin (/std/first-good-value `(
                                   ~(/prelude/extern-pointer libc "stdin" 'pointer)
                                   ~(/prelude/extern-pointer libc "__stdinp" 'pointer))))

(def stdout (/std/first-good-value `(
                                    ~(/prelude/extern-pointer libc "stdout" 'pointer)
                                    ~(/prelude/extern-pointer libc "__stdoutp" 'pointer))))

(def stderr (/std/first-good-value `(
                                    ~(/prelude/extern-pointer libc "stderr" 'pointer)
                                    ~(/prelude/extern-pointer libc "__stderrp" 'pointer))))

(defn print (val)
  (return (/prelude/fprintf-bytestring stdout "%s\n" (/std/repr val))))

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
