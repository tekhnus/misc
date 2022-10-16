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
 (third "std" third)
 (fourth "std" fourth))

!(req
  (defun "stdmacro" defun)
  (fn "stdmacro" fn)
  (def2 "stdmacro" def2))

(def compdata-make (c-function-or-panic selflib "compdata_make" '(() pointer)))
(def make-routine-with-empty-state (c-function-or-panic selflib "make_routine_0_with_empty_state" '((sizet) pointer)))
(def prog-slice-make (c-function-or-panic selflib "prog_slice_make" '((sizet) progslice)))
(def prog-slice-append-new- (c-function-or-panic selflib "prog_slice_append_new" '((pointer) sizet)))
(def prog-init-module-c-host (c-function-or-panic selflib "prog_build_one_c_host" '((pointer sizet datum pointer) pointer)))
(def decode-offset (c-function-or-panic selflib "decode_offset_from_routine_0" '((pointer) sizet)))
!(#defun prog-slice-append-new (sl)
   (return (prog-slice-append-new- (wrap-pointer-into-pointer sl))))

!(#defun compile-prog-new (sl p src compdata)
   (progn
     (def e (prog-init-module-c-host (wrap-pointer-into-pointer sl) p src (wrap-pointer-into-pointer compdata)))
     (if (eq 0 (derefw `(~e int64)))
         (return `(:ok ~p))
       (return `(:err ~(derefw `(~e string)))))))


(def routine-run-and-get-value-c-host-new (c-function-or-panic selflib "routine_run_and_get_value_c_host_new" '((progslice pointer) fdatum)))
(def fdatum-is-panic (c-function-or-panic selflib "fdatum_is_panic" '((fdatum) int)))
(builtin.defn fdatum-get-value (return (derefw `(~(head args) val))))
(def fdatum-get-panic-message (c-function-or-panic selflib "fdatum_get_panic_message" '((fdatum) string)))

!(#defun eval-new (sl rt0)
   (progn
     (def rt-ptr (wrap-pointer-into-pointer rt0))
     (def res (routine-run-and-get-value-c-host-new sl rt-ptr))
     (if (eq (fdatum-is-panic res) 1)
         (progn
           (def msg (fdatum-get-panic-message res))
           (return `(:err ~msg)))
       (progn
         (def val (fdatum-get-value res))
         (def new-rt (host "deref" `(~rt-ptr int64)))
         (return `(:ok ~val ~new-rt))))))

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

(export (compile-prog-new compile-prog-new)
        (eval-new eval-new)
        (read read)
        (make-routine-with-empty-state make-routine-with-empty-state)
        (decode-offset decode-offset)
        (prog-slice-make prog-slice-make)
        (prog-slice-append-new prog-slice-append-new)
        (compdata-make compdata-make))
