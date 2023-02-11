(req
 (prelude "prelude")
 (shared-library "prelude" shared-library)
 (c-function "prelude" c-function)
 (selflib "prelude" selflib)
 (dlsym "prelude" dlsym)
 (dereference "prelude" dereference)
 (wrap-pointer-into-pointer "prelude" wrap-pointer-into-pointer)
 (call-extension-1 "prelude" call-extension-1)
 (std "std")
 (decons-pat "std" decons-pat)
 (first-good-value "std" first-good-value)
 (eq "std" eq)
 (head "std" head)
 (tail "std" tail)
 (panic "std" panic))

(def buildlib (/std/first-good-value `(
                                      ~(/prelude/shared-library "libzlisp-build-lib.so"))))

(def compdata-make (/prelude/c-function selflib "compdata_make" '(() pointer)))
(def make-routine-with-empty-state (/prelude/c-function selflib "routine_make" '((sizet) pointer)))
(def prog-slice-make (/prelude/c-function selflib "vec_make" '((sizet) progslice)))
(def prog-slice-append-new- (/prelude/c-function selflib "vec_append_new" '((pointer) sizet)))
(def prog-build-one-c-host (/prelude/c-function buildlib "prog_build" '((pointer pointer pointer pointer pointer pointer pointer) pointer)))
(def prog-build-init (/prelude/c-function buildlib "prog_build_init" '((pointer pointer pointer pointer pointer) sizet)))
(def get-host-ffi-settings (/prelude/c-function buildlib "get_host_ffi_settings" '(() pointer)))

(defn prog-slice-append-new (sl)
  (return (/prelude/prog-slice-append-new- (/prelude/wrap-pointer-into-pointer sl))))

(defn init-prog (sl pptr bpptr compdata bdrcompdata)
  (progn
    (def nothing (/prelude/prog-build-init (/prelude/wrap-pointer-into-pointer sl) (/prelude/wrap-pointer-into-pointer pptr) (/prelude/wrap-pointer-into-pointer bpptr) (/prelude/wrap-pointer-into-pointer compdata) (/prelude/wrap-pointer-into-pointer bdrcompdata)))
    (return 42)))

(defn compile-prog-new (sl pptr bpptr src compdata bdrcompdata)
  (progn
    (def e (/prelude/prog-build-one-c-host (/prelude/wrap-pointer-into-pointer sl) (/prelude/wrap-pointer-into-pointer pptr) (/prelude/wrap-pointer-into-pointer bpptr) (/prelude/wrap-pointer-into-pointer src) (/prelude/wrap-pointer-into-pointer compdata) (/prelude/wrap-pointer-into-pointer bdrcompdata) (/prelude/get-host-ffi-settings)))
    (if (/std/eq 0 (/prelude/dereference e 'int64))
        (return `(:ok :nothing))
      (return `(:err ~(/prelude/dereference e 'string))))))


(def routine-run-and-get-value-c-host-new (/prelude/c-function selflib "routine_run_in_ffi_host" '((progslice pointer) fdatum)))
(def fdatum-is-panic (/prelude/c-function selflib "fdatum_is_panic" '((fdatum) int)))

(def fdatum-get-value-ptr (/prelude/dlsym selflib "fdatum_get_value"))
(defn fdatum-get-value (x) (return (/prelude/call-extension-1 (/prelude/dereference fdatum-get-value-ptr 'int64) x)))

(def fdatum-get-panic-message-ptr (/prelude/dlsym selflib "fdatum_get_panic_message"))
(defn fdatum-get-panic-message (x) (return (/prelude/call-extension-1 (/prelude/dereference fdatum-get-panic-message-ptr 'int64) x)))

(def fdatum-repr-datum-pointer-ptr (/prelude/dlsym selflib "fdatum_repr_datum_pointer"))
(defn repr-pointer (x) (return (/prelude/call-extension-1 (/prelude/dereference fdatum-repr-datum-pointer-ptr 'int64) x)))

(defn eval-new (sl rt0)
  (progn
    (def res (/prelude/routine-run-and-get-value-c-host-new sl rt0))
    (if (/std/eq (/prelude/fdatum-is-panic res) 1)
        (progn
          (def msg (../fdatum-get-panic-message res))
          (return `(:err ~msg)))
      (progn
        (def val (../fdatum-get-value res))
        (return `(:ok ~val ~rt0))))))

(def datum-read-one (/prelude/c-function selflib "datum_read_one" '((pointer) fdatum)))

(defn read (strm)
  (progn
    (def res (/prelude/datum-read-one strm))
    (if (/std/eq (/prelude/fdatum-is-panic res) 1)
        (progn
          (def msg (../fdatum-get-panic-message res))
          (if (/std/eq msg "eof")
              (return '(:eof))
            (return `(:err ~msg))))
      (progn
        (def maybeval (../fdatum-get-value res))
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
