(req)

(def panic-pointer (host "panic" '()))
(builtin.defn panic (return (host "deref" `(~(host "pointer-call-datums" `(~panic-pointer  ((datum) val)  ~args)) val))))

(def head-pointer (host "head" '()))
(builtin.defn head (return (host "deref" `(~(host "pointer-call-datums" `(~head-pointer  ((datum) val)  ~args)) val))))

(def tail-pointer (host "tail" '()))
(builtin.defn tail (return (host "deref" `(~(host "pointer-call-datums" `(~tail-pointer  ((datum) val)  ~args)) val))))

(def cons-pointer (host "cons" '()))
(builtin.defn cons (return (host "deref" `(~(host "pointer-call-datums" `(~cons-pointer  ((datum datum) val)  ~args)) val))))

(def eq-pointer (host "eq" '()))
(builtin.defn eq (return (host "deref" `(~(host "pointer-call-datums" `(~eq-pointer  ((datum datum) val)  ~args)) val))))

(builtin.defn serialize-param
              (progn
                (def param (head args))
                (def signature (head (tail args)))
                (if (eq signature 'pointer)
                    (return param)
                  (if (eq signature 'fdatum)
                      (return param)
                    (if (eq signature 'progslice)
                        (return param)
                      (return (host "mkptr" `(~param ~signature))))))))

(builtin.defn serialize-params
              (progn
                (def params (head args))
                (def signature (head (tail args)))
                (if params
                    (return (cons (serialize-param (head params) (head signature)) (serialize-params (tail params) (tail signature))))
                  (return '()))))

(builtin.defn derefw
              (progn
                (def whathow (head args))
                (def what (head whathow))
                (def how (head (tail whathow)))
                (if (eq how 'pointer)
                    (return what)
                  (if (eq how 'fdatum)
                      (return what)
                    (if (eq how 'progslice)
                        (return what)
                      (return (host "deref" whathow)))))))

(builtin.defn pointer-call-and-deserialize
              (progn
                (def annotated-function-and-params (head args))
                (def annotated-function (head annotated-function-and-params))
                (def params (head (tail annotated-function-and-params)))
                (def annotation annotated-function)
                (def fn-ptr (head annotation))
                (def signature (head (tail annotation)))
                (def fnparamst (head signature))
                (def rettype (head (tail signature)))
                (def s (serialize-params params fnparamst))
                (def rawres (host "pointer-call" `(~fn-ptr (~fnparamst ~rettype) ~s)))
                (return (derefw `(~rawres ~rettype)))))


(def dlopen-pointer (host "dlopen" '()))
(builtin.defn dlopen (return (pointer-call-and-deserialize `((~dlopen-pointer ((string) pointer)) ~args))))

(def dlsym-pointer (host "dlsym" '()))
(builtin.defn dlsym (return (pointer-call-and-deserialize `((~dlsym-pointer ((pointer string) pointer)) ~args))))

(builtin.defn c-data-pointer
            (progn
              (def handle (head args))
                (def c-name (head (tail args)))
                (def signature (head (tail (tail args))))
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (derefw `(~fn-pointer-pointer int64)))
                (return fn-pointer)))    

(builtin.defn c-function-or-panic
              (progn
                (def handle (head args))
                (def c-name (head (tail args)))
                (def signature (head (tail (tail args))))
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-ptr (derefw `(~fn-pointer-pointer int64)))
                (if (eq fn-ptr 0)
                    (panic "couldn't load C function")
                  (return
                   (builtin.fn
                    (return (pointer-call-and-deserialize `((~fn-ptr ~signature) ~args))))))))
                


(def selflib (dlopen ""))

(builtin.defn builtin-function
              (progn
                (def c-name (head args))
                (def signature (head (tail args)))
                (return (c-function-or-panic selflib c-name signature))))

(def eq (builtin-function "builtin_eq" '((datum datum) val)))
(def annotate (builtin-function "builtin_annotate" '((datum) val)))
(def is-constant (builtin-function "builtin_is_constant" '((datum) val)))
(def repr (builtin-function "builtin_repr" '((datum) val)))
(def concat-bytestrings (builtin-function "builtin_concat_bytestrings" '((datum datum) val)))
(def + (builtin-function "builtin_add" '((datum datum) val)))

(builtin.defn wrap-pointer-into-pointer (return (host "mkptr" `(~(head args) sizet))))


(builtin.defn shared-library (progn
                               (def r (dlopen (head args)))
                               (if (eq 0 (derefw `(~r int64)))
                                   (return `(:err "shared-library failed"))
                                 (return `(:ok ~r)))))

(builtin.defn extern-pointer (progn
                               (def handle (head args))
                               (def c-name (head (tail args)))
                               (def signature (head (tail (tail args))))
                               (def res (c-data-pointer handle c-name signature))
                               (if (eq 0 res)
                                   (return `(:err "extern-pointer failed"))
                                 (return `(:ok ~res)))))
                                        
(builtin.defn debug-print (return '()))

(export
 (panic panic)
 (head head)
 (tail tail)
 (cons cons)
 (eq eq)
 (derefw derefw)
 (dlopen dlopen)
 (dlsym dlsym)
 (c-function-or-panic c-function-or-panic)
 (eq eq)
 (annotate annotate)
 (is-constant is-constant)
 (repr repr)
 (concat-bytestrings concat-bytestrings)
 (+ +)
 (wrap-pointer-into-pointer wrap-pointer-into-pointer)
 (shared-library shared-library)
 (extern-pointer extern-pointer)
 (selflib selflib))
