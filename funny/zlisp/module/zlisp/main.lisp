!(require "stdmacro")
(require "std")

!(#def-or-panica zlisp-zlisp
  (shared-library "libzlisp-impl-wrapper.so")
  (shared-library "libzlisp-impl-wrapper.dylib"))

!(#def-or-panica read-
     (extern-pointer zlisp-zlisp "read" '((datum) val)))
!(#wrap-fn-pointer read read-)

!(#def-or-panica eval-
     (extern-pointer zlisp-zlisp "eval" '((datum datum) val)))
!(#wrap-fn-pointer eval eval-)

!(#def-or-panica builtins-
  (extern-pointer zlisp-zlisp "builtins" '(() val)))
!(#wrap-fn-pointer builtins builtins-)
