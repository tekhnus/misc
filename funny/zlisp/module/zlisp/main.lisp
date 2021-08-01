(def-or-panic zlisp-zlisp (shared-library "libzlisp-impl-wrapper.dylib"))

(def-or-panic read
     (extern-pointer zlisp-zlisp "read" '((datum) eval_result)))

(def-or-panic eval
     (extern-pointer zlisp-zlisp "eval" '((datum datum) eval_result)))

(def-or-panic prelude
  (extern-pointer zlisp-zlisp "prelude" '(() eval_result)))
