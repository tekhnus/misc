(req
 (prelude "prelude")
 (shared-library "prelude" shared-library)
 (c-function-or-panic "prelude" c-function-or-panic)
 (c-function-or-panic-new "prelude" c-function-or-panic-new)
 (selflib "prelude" selflib)
 (dlsym "prelude" dlsym)
 (derefw2 "prelude" derefw2)
 (wrap-pointer-into-pointer "prelude" wrap-pointer-into-pointer)
 (std "std")
 (decons-pat "std" decons-pat)
 (first-good-value "std" first-good-value)
 (eq "std" eq)
 (head "std" head)
 (tail "std" tail)
 (panic "std" panic))

(def buildlib (std @slash first-good-value `(
  ~(prelude @slash shared-library "libzlisp-build-lib.so"))))

(def compdata-make (prelude @slash c-function-or-panic-new selflib "compdata_make" '(() pointer)))
(def make-routine-with-empty-state (prelude @slash c-function-or-panic-new selflib "routine_make_new" '((sizet) pointer)))
(def prog-slice-make (prelude @slash c-function-or-panic-new selflib "prog_slice_make" '((sizet) progslice)))
(def prog-slice-append-new- (prelude @slash c-function-or-panic-new selflib "prog_slice_append_new" '((pointer) sizet)))
(def prog-build-one-c-host (prelude @slash c-function-or-panic-new buildlib "prog_build" '((pointer pointer pointer pointer pointer pointer pointer) pointer)))
(def prog-build-init (prelude @slash c-function-or-panic-new buildlib "prog_build_init" '((pointer pointer pointer pointer pointer) sizet)))
(def get-host-ffi-settings (prelude @slash c-function-or-panic-new buildlib "get_host_ffi_settings" '(() pointer)))

(builtin.defun prog-slice-append-new (sl)
   (return (prelude @slash prog-slice-append-new- (prelude @slash wrap-pointer-into-pointer sl))))

(builtin.defun init-prog (sl pptr bpptr compdata bdrcompdata)
   (progn
     (def nothing (prelude @slash prog-build-init (prelude @slash wrap-pointer-into-pointer sl) (prelude @slash wrap-pointer-into-pointer pptr) (prelude @slash wrap-pointer-into-pointer bpptr) (prelude @slash wrap-pointer-into-pointer compdata) (prelude @slash wrap-pointer-into-pointer bdrcompdata)))
     (return 42)))

(builtin.defun compile-prog-new (sl pptr bpptr src compdata bdrcompdata)
   (progn
     (def e (prelude @slash prog-build-one-c-host (prelude @slash wrap-pointer-into-pointer sl) (prelude @slash wrap-pointer-into-pointer pptr) (prelude @slash wrap-pointer-into-pointer bpptr) (prelude @slash wrap-pointer-into-pointer src) (prelude @slash wrap-pointer-into-pointer compdata) (prelude @slash wrap-pointer-into-pointer bdrcompdata) (prelude @slash get-host-ffi-settings)))
     (if (std @slash eq 0 (prelude @slash derefw2 e 'int64))
         (return `(:ok :nothing))
       (return `(:err ~(prelude @slash derefw2 e 'string))))))


(def routine-run-and-get-value-c-host-new (prelude @slash c-function-or-panic selflib "routine_run_and_get_value_c_host_new_new" '((progslice pointer) fdatum)))
(def fdatum-is-panic (prelude @slash c-function-or-panic selflib "fdatum_is_panic" '((fdatum) int)))

(def fdatum-get-value-ptr (prelude @slash dlsym selflib "fdatum_get_value"))
(builtin.defun fdatum-get-value (x) (return (host "call-extension" (prelude @slash derefw2 fdatum-get-value-ptr 'int64) x)))

(def fdatum-get-panic-message-ptr (prelude @slash dlsym selflib "fdatum_get_panic_message"))
(builtin.defun fdatum-get-panic-message (x) (return (host "call-extension" (prelude @slash derefw2 fdatum-get-panic-message-ptr 'int64) x)))

(def fdatum-repr-datum-pointer-ptr (prelude @slash dlsym selflib "fdatum_repr_datum_pointer"))
(builtin.defun repr-pointer (x) (return (host "call-extension" (prelude @slash derefw2 fdatum-repr-datum-pointer-ptr 'int64) x)))

(builtin.defun eval-new (sl rt0)
   (progn
     (def rt-ptr (prelude @slash wrap-pointer-into-pointer rt0))
     (def res (routine-run-and-get-value-c-host-new sl rt-ptr))
     (if (std @slash eq (fdatum-is-panic res) 1)
         (progn
           (def msg (fdatum-get-panic-message res))
           (return `(:err ~msg)))
       (progn
         (def val (fdatum-get-value res))
         (def new-rt (prelude @slash derefw2 rt-ptr 'int64))
         (return `(:ok ~val ~new-rt))))))

(def datum-read-one (prelude @slash c-function-or-panic selflib "datum_read_one" '((pointer) fdatum)))
(builtin.defun read (strm)
   (progn
     (def res (datum-read-one strm))
     (if (std @slash eq (fdatum-is-panic res) 1)
         (progn
           (def msg (fdatum-get-panic-message res))
           (if (std @slash eq msg "eof")
               (return '(:eof))
             (return `(:err ~msg))))
       (progn
         (def maybeval (fdatum-get-value res))
         (return `(:ok ~maybeval))))))

(export (compile-prog-new compile-prog-new)
        (init-prog init-prog)
        (eval-new eval-new)
        (read read)
        (repr-pointer repr-pointer)
        (make-routine-with-empty-state make-routine-with-empty-state)
        (prog-slice-make prog-slice-make)
        (prog-slice-append-new prog-slice-append-new)
        (compdata-make compdata-make))
