!(require "stdmacro")

!(#def-or-panica zlisp-zlisp
  (shared-library "libzlisp-impl-wrapper.so")
  (shared-library "libzlisp-impl-wrapper.dylib"))

!(#def-or-panica read
     (extern-pointer zlisp-zlisp "read" '((datum) val)))

!(#def-or-panica eval
     (extern-pointer zlisp-zlisp "eval" '((datum datum) val)))

!(#def-or-panica prelude
  (extern-pointer zlisp-zlisp "prelude" '(() val)))
