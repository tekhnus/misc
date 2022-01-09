!(require "stdmacro")
(require "std")

!(#def-or-panica zlisp-zlisp
  (shared-library "libzlisp-impl-wrapper.so")
  (shared-library "libzlisp-impl-wrapper.dylib"))

!(#def-or-panica read-
     (extern-pointer zlisp-zlisp "read" '((datum) val)))
!(#wrap-fn-pointer read read-)


(def selflib (dlopen-or-panic ""))
(def builtins (c-function-or-panic selflib "state_make_builtins" '(() pointer)))
(def compile-prog (c-function-or-panic selflib "compile_prog" '((datum) pointer)))
!(#defun compile-statement (s) (return (compile-prog `(~s))))
(def routine-run-and-get-value-c-host-fdatum (c-function-or-panic selflib "routine_run_and_get_value_c_host" '((pointer pointer) fdatum)))
(def fdatum-is-panic (c-function-or-panic selflib "fdatum_is_panic" '((fdatum) int)))
(def fdatum-get-value (c-function-or-panic selflib "fdatum_get_value" '((fdatum) val)))
(def fdatum-get-panic-message (c-function-or-panic selflib "fdatum_get_panic_message" '((fdatum) string)))
!(#defun eval (state prog)
   (progn
     (def state-ptr (wrap-pointer-into-pointer state))
     (def res (routine-run-and-get-value-c-host-fdatum state-ptr prog))
     (if (eq (fdatum-is-panic res) 1)
         (progn
           (def msg (fdatum-get-panic-message res))
           (return `(:err ~msg)))
       (progn
         (def val (fdatum-get-value res))
         (def new-state (dereference-and-cast state-ptr 'pointer))
         (return `(:ok ~val ~new-state))))))
