(def dlopen-pointer `(cptr (~(host "dlopen" '()) ((string) pointer))))
(builtin.defn dlopen (return `(cptr (~(host "pointer-call" `(~dlopen-pointer ~args)) pointer))))

(def dlsym-pointer `(cptr (~(host "dlsym" '()) ((pointer string) pointer))))
(builtin.defn dlsym (return `(cptr (~(host "pointer-call" `(~dlsym-pointer ~args)) pointer))))

(def dereference-and-cast-pointer `(cptr (~(host "dereference-and-cast" '()) ((datum datum) val))))
(builtin.defn dereference-and-cast (return (host "dereference-datum" (host "pointer-call" `(~dereference-and-cast-pointer ~args)))))

(def not-null-pointer-ptr `(cptr (~(host "not-null-pointer" '()) ((datum) val))))
(builtin.defn not-null-pointer (return (host "dereference-datum" (host "pointer-call" `(~not-null-pointer-ptr ~args)))))

(def wrap-pointer-into-pointer-ptr `(cptr (~(host "wrap-pointer-into-pointer" '()) ((datum) val))))
(builtin.defn wrap-pointer-into-pointer (return (host "dereference-datum" (host "pointer-call" `(~wrap-pointer-into-pointer-ptr ~args)))))

(def panic-pointer `(cptr (~(host "panic" '()) ((datum) val))))
(builtin.defn panic (return (host "dereference-datum" (host "pointer-call" `(~panic-pointer ~args)))))

(def head-pointer `(cptr (~(host "head" '()) ((datum) val))))
(builtin.defn head (return (host "dereference-datum" (host "pointer-call" `(~head-pointer ~args)))))

(def tail-pointer `(cptr (~(host "tail" '()) ((datum) val))))
(builtin.defn tail (return (host "dereference-datum" (host "pointer-call" `(~tail-pointer ~args)))))

(builtin.defn c-function-pointer
            (progn
              (def handle (head args))
                (def c-name (head (tail args)))
                (def signature (head (tail (tail args))))
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (dereference-and-cast fn-pointer-pointer signature))
                (return fn-pointer)))             

(builtin.defn shared-library (progn
                               (def res-ptr (dlopen (head args)))
                               (def res (dereference-and-cast res-ptr 'pointer))
                               (if (not-null-pointer res)
                                   (return `(:ok ~res-ptr))
                                 (return `(:err "shared-library failed")))))

(builtin.defn extern-pointer (progn
                               (def handle (head args))
                               (def c-name (head (tail args)))
                               (def signature (head (tail (tail args))))
                               (def res (c-function-pointer handle c-name signature))
                               (if (not-null-pointer res)
                                   (return `(:ok ~res))
                                 (return `(:err "extern-pointer failed")))))

(builtin.defn c-function-or-panic
              (progn
                (def handle (head args))
                (def c-name (head (tail args)))
                (def signature (head (tail (tail args))))
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (dereference-and-cast fn-pointer-pointer signature))
                (if (not-null-pointer fn-pointer)
                    ((def fn-routine (builtin.fn (return (host "pointer-call-old" `(~fn-pointer ~args)))))
                     (return fn-routine))
                  (panic (concat-bytestrings "couldn't load C function " c-name)))))

(def selflib (dlopen ""))

(builtin.defn builtin-function
              (progn
                (def c-name (head args))
                (def signature (head (tail args)))
                (return (c-function-or-panic selflib c-name signature))))

(def cons (builtin-function "builtin_cons" '((datum datum) val)))
(def panic (builtin-function "builtin_panic" '((datum) val)))
(def eq (builtin-function "builtin_eq" '((datum datum) val)))
(def annotate (builtin-function "builtin_annotate" '((datum) val)))
(def is-constant (builtin-function "builtin_is_constant" '((datum) val)))
(def repr (builtin-function "builtin_repr" '((datum) val)))
(def concat-bytestrings (builtin-function "builtin_concat_bytestrings" '((datum datum) val)))
(def + (builtin-function "builtin_add" '((datum datum) val)))


(builtin.defn wrap-fn-pointer (return `(def ~(head args) (builtin.fn (return (host "pointer-call-old" (list ~(head (tail args)) args)))))))
