(req
 (shared-library "prelude" shared-library)
 (c-function-or-panic "prelude" c-function-or-panic)
 (extern-pointer "prelude" extern-pointer)
 (selflib "prelude" selflib)
 (derefw "prelude" derefw)
 (wrap-pointer-into-pointer "prelude" wrap-pointer-into-pointer)
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (tail "std" tail)
 (panic "std" panic)
 (second "std" second)
 (third "std" third))

!(req
  (defun "stdmacro" defun)
  (fn "stdmacro" fn)
  (def2 "stdmacro" def2)
  (def-or-panica "stdmacro" def-or-panica))

(def compdata-make (c-function-or-panic selflib "compdata_make" '(() pointer)))
(def builtins (c-function-or-panic selflib "state_make_builtins" '(() pointer)))
(def prog-slice-make (c-function-or-panic selflib "prog_slice_make" '((sizet) progslice)))
(def prog-slice-append-new (c-function-or-panic selflib "prog_slice_append_new" '((pointer) sizet)))
(def prog-init-module-c-host (c-function-or-panic selflib "prog_build_one_c_host" '((pointer sizet datum pointer) pointer)))
!(#defun compile-prog (sl src compdata)
   (progn
     (def p (prog-slice-append-new (wrap-pointer-into-pointer sl)))
     (def e (prog-init-module-c-host (wrap-pointer-into-pointer sl) p src (wrap-pointer-into-pointer compdata)))
     (if (eq 0 (derefw `(~e int64)))
         (return `(:ok ~p))
       (return `(:err ~(derefw `(~e string)))))))


(def routine-run-and-get-value-c-host-fdatum (c-function-or-panic selflib "routine_run_and_get_value_c_host" '((progslice pointer sizet) fdatum)))
(def fdatum-is-panic (c-function-or-panic selflib "fdatum_is_panic" '((fdatum) int)))
(builtin.defn fdatum-get-value (return (derefw `(~(head args) val))))
(def fdatum-get-panic-message (c-function-or-panic selflib "fdatum_get_panic_message" '((fdatum) string)))
!(#defun eval (sl state prog)
   (progn
     (def state-ptr (wrap-pointer-into-pointer state))
     (def res (routine-run-and-get-value-c-host-fdatum sl state-ptr prog))
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

(export (compile-prog compile-prog)
        (eval eval)
        (read read)
        (builtins builtins)
        (prog-slice-make prog-slice-make)
        (compdata-make compdata-make))
