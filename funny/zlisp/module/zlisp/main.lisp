!(require "stdmacro")
(require "std")

(def builtins (c-function-or-panic selflib "state_make_builtins" '(() pointer)))
(def prog-make (c-function-or-panic selflib "prog_make" '(() pointer)))
(def prog-init-module-c-host (c-function-or-panic selflib "prog_init_module_c_host" '((pointer datum) pointer)))
!(#defun compile-prog (src)
   (progn
     (def p (prog-make))
     (def e (prog-init-module-c-host p src))
     (if (eq 0 e)
         (return `(:err "some compilation error"))
       (return `(:ok ~p)))))

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
         (def new-state (host "deref" `(~state-ptr int64)))
         (return `(:ok ~val ~new-state))))))
(def datum-read-one (c-function-or-panic selflib "datum_read_one" '((pointer) fdatum)))
!(#defun read (strm)
   (progn
     (def res (datum-read-one strm))
     (if (eq (fdatum-is-panic res) 1)
         (progn
           (def msg (fdatum-get-panic-message res))
           (return `(:err ~msg)))
       (progn
         (def maybeval (fdatum-get-value res))
         (if maybeval
             (progn
               (def val (head maybeval))
               (return `(:ok ~val)))
           (return '(:eof)))))))
