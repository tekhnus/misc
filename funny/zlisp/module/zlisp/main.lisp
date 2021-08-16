!(#def-or-panica zlisp-zlisp
  (shared-library "libzlisp-impl-wrapper.so")
  (shared-library "libzlisp-impl-wrapper.dylib"))

!(#def-or-panica read
     (extern-pointer zlisp-zlisp "read" '((datum) eval_result)))

!(#def-or-panica eval
     (extern-pointer zlisp-zlisp "eval" '((datum datum) eval_result)))

!(#def-or-panica prelude
  (extern-pointer zlisp-zlisp "prelude" '(() eval_result)))
