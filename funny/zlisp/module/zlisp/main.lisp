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


(def selflib (dlopen-or-panic ""))
(def builtins (c-function-or-panic selflib "state_make_builtins" '(() pointer)))
(def compile-prog (c-function-or-panic selflib "compile_prog" '((datum) pointer)))
!(#defun compile-statement (s) (return (compile-prog `(~s))))
(def routine-run-and-get-value-c-host (c-function-or-panic selflib "routine_run_and_get_value_c_host" '((pointer pointer) val)))
