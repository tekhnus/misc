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
(def routine-run-and-get-value-c-host-fdatum (c-function-or-panic selflib "routine_run_and_get_value_c_host" '((pointer pointer) fdatum)))
(def fdatum-is-panic (c-function-or-panic selflib "fdatum_is_panic" '((fdatum) int)))
!(#defun routine-run-and-get-value-c-host (state-ptr prog)
   (progn
     (def res (routine-run-and-get-value-c-host-fdatum state-ptr prog))
     (if (eq (fdatum-is-panic res) 1)
         (return `(:err "eval failed"))
       (return `(:ok)))))

(panic (repr (routine-run-and-get-value-c-host (wrap-pointer-into-pointer (builtins)) (compile-prog '((def x 123))))))
