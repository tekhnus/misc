(def panic-pointer (host "panic" '()))
(builtin.defn panic (return (host "dereference-datum" (host "pointer-call-datums" `((cptr (~panic-pointer  ((datum) val)))  ~args)))))

(def head-pointer (host "head" '()))
(builtin.defn head (return (host "dereference-datum" (host "pointer-call-datums" `((cptr (~head-pointer  ((datum) val)))  ~args)))))

(def tail-pointer (host "tail" '()))
(builtin.defn tail (return (host "dereference-datum" (host "pointer-call-datums" `((cptr (~tail-pointer  ((datum) val)))  ~args)))))

(def cons-pointer (host "cons" '()))
(builtin.defn cons (return (host "dereference-datum" (host "pointer-call-datums" `((cptr (~cons-pointer  ((datum datum) val)))  ~args)))))

(builtin.defn serialize-param
              (progn
                (def param (head args))
                (def signature (head (tail args)))
                (return (host "mkptr" `(~param ~signature)))))

(builtin.defn serialize-params
              (progn
                (def params (head args))
                (def signature (head (tail args)))
                (if params
                    (return (cons (serialize-param (head params) (head signature)) (serialize-params (tail params) (tail signature))))
                  (return '()))))

(builtin.defn ptr-call
              (progn
                (def annotated-function-and-params (head args))
                (def annotated-function (head annotated-function-and-params))
                (def params (head (tail annotated-function-and-params)))
                (def annotation (head (tail annotated-function)))
                (def fn-ptr (head annotation))
                (def signature (head (tail annotation)))
                (def fnparamst (head signature))
                (def rettype (head (tail signature)))
                (def s (serialize-params params fnparamst))
                (def rawres (host "pointer-call" `((cptr (~fn-ptr (~fnparamst ~rettype))) ~s)))
                (return rawres)))

(builtin.defn pointer-call-and-deserialize
              (progn
                (def annotated-function-and-params (head args))
                (def annotated-function (head annotated-function-and-params))
                (def params (head (tail annotated-function-and-params)))
                (def annotation (head (tail annotated-function)))
                (def fn-ptr (head annotation))
                (def signature (head (tail annotation)))
                (def fnparamst (head signature))
                (def rettype (head (tail signature)))
                (def s (serialize-params params fnparamst))
                (def rawres (host "pointer-call" `((cptr (~fn-ptr (~fnparamst ~rettype))) ~s)))
                (return (host "deref" `(~rawres ~rettype)))))


(def dlopen-pointer `(cptr (~(host "dlopen" '()) ((string) pointer))))
(builtin.defn dlopen (return `(cptr (~(ptr-call `(~dlopen-pointer ~args)) pointer))))
(builtin.defn dlopen-new (return (ptr-call `(~dlopen-pointer ~args))))

(def dlsym-pointer `(cptr (~(host "dlsym" '()) ((pointer string) pointer))))
(builtin.defn dlsym (return `(cptr (~(ptr-call `(~dlsym-pointer ~args)) pointer))))

(def dereference-and-cast-pointer `(cptr (~(host "dereference-and-cast" '()) ((datum datum) val))))
(builtin.defn dereference-and-cast (return (host "dereference-datum" (ptr-call `(~dereference-and-cast-pointer ~args)))))

(def dereference-and-castdat-pointer `(cptr (~(host "dereference-and-castdat" '()) ((datum datum) val))))
(builtin.defn dereference-and-castdat (return (host "dereference-datum" (ptr-call `(~dereference-and-castdat-pointer ~args)))))

(def not-null-pointer-ptr `(cptr (~(host "not-null-pointer" '()) ((datum) val))))
(builtin.defn not-null-pointer (return (host "dereference-datum" (ptr-call `(~not-null-pointer-ptr ~args)))))

(def nonzero-ptr `(cptr (~(host "nonzero" '()) ((datum) val))))
(builtin.defn nonzero (return (host "dereference-datum" (ptr-call `(~nonzero-ptr ~args)))))

(def wrap-pointer-into-pointer-ptr `(cptr (~(host "wrap-pointer-into-pointer" '()) ((datum) val))))
(builtin.defn wrap-pointer-into-pointer (return (host "dereference-datum" (ptr-call `(~wrap-pointer-into-pointer-ptr ~args)))))

(builtin.defn c-function-pointer
            (progn
              (def handle (head args))
                (def c-name (head (tail args)))
                (def signature (head (tail (tail args))))
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (dereference-and-cast fn-pointer-pointer signature))
                (return fn-pointer)))             

(builtin.defn shared-library (progn
                               (def res-ptr (dlopen-new (head args)))
                               (if (nonzero res-ptr)
                                   (return `(:ok (cptr (~res-ptr pointer))))
                                 (return `(:err "shared-library failed")))))

(builtin.defn extern-pointer (progn
                               (def handle (head args))
                               (def c-name (head (tail args)))
                               (def signature (head (tail (tail args))))
                               (def res (c-function-pointer handle c-name signature))
                               (if (not-null-pointer res)
                                   (return `(:ok ~res))
                                 (return `(:err "extern-pointer failed")))))

(builtin.defn pointer-call-and-interpret
              (progn
                (def rawres (ptr-call (head args)))
                (return (host "dereference-datum" rawres))))

(builtin.defn builtin-or-panic
              (progn
                (def handle (head args))
                (def c-name (head (tail args)))
                (def signature (head (tail (tail args))))
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (dereference-and-cast fn-pointer-pointer signature))
                (if (not-null-pointer fn-pointer)
                    ((def fn-routine (builtin.fn (return (pointer-call-and-interpret `(~fn-pointer ~args)))))
                     (return fn-routine))
                  (panic c-name))))

(def selflib (dlopen ""))

(builtin.defn builtin-function
              (progn
                (def c-name (head args))
                (def signature (head (tail args)))
                (return (builtin-or-panic selflib c-name signature))))

(def eq (builtin-function "builtin_eq" '((datum datum) val)))
(def annotate (builtin-function "builtin_annotate" '((datum) val)))
(def is-constant (builtin-function "builtin_is_constant" '((datum) val)))
(def repr (builtin-function "builtin_repr" '((datum) val)))
(def concat-bytestrings (builtin-function "builtin_concat_bytestrings" '((datum datum) val)))
(def + (builtin-function "builtin_add" '((datum datum) val)))

(builtin.defn c-function-or-panic
              (progn
                (def handle (head args))
                (def c-name (head (tail args)))
                (def signature (head (tail (tail args))))
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (dereference-and-cast fn-pointer-pointer signature))
                (if (not-null-pointer fn-pointer)
                    ((def fn-routine (builtin.fn (return (pointer-call-and-deserialize `(~fn-pointer ~args)))))
                     (return fn-routine))
                  (panic (concat-bytestrings "couldn't load C function " c-name)))))

(builtin.defn wrap-fn-pointer (return `(def ~(head args) (builtin.fn (return (pointer-call-and-deserialize (list ~(head (tail args)) args)))))))
