(req)

(def panic-pointer (host "panic" '()))
(builtin.defn panic (arg) (return (host "deref" `(~(host "pointer-call-datums" `(~panic-pointer  ((datum) val)  (~arg))) val))))

(def head-pointer (host "head" '()))
(builtin.defn head (arg) (return (host "deref" `(~(host "pointer-call-datums" `(~head-pointer  ((datum) val)  (~arg))) val))))

(def tail-pointer (host "tail" '()))
(builtin.defn tail (arg) (return (host "deref" `(~(host "pointer-call-datums" `(~tail-pointer  ((datum) val)  ~(arg))) val))))

(def cons-pointer (host "cons" '()))
(builtin.defn cons (arg) (return (host "deref" `(~(host "pointer-call-datums" `(~cons-pointer  ((datum datum) val)  ~(arg))) val))))

(def eq-pointer (host "eq" '()))
(builtin.defn eq (arg) (return (host "deref" `(~(host "pointer-call-datums" `(~eq-pointer  ((datum datum) val)  ~(arg))) val))))

(builtin.defn serialize-param (param signature)
              (progn
                (if (eq signature 'pointer)
                    (return param)
                  (if (eq signature 'fdatum)
                      (return param)
                    (if (eq signature 'progslice)
                        (return param)
                      (return (host "mkptr" `(~param ~signature))))))))

(builtin.defn serialize-params (params signature)
              (progn
                (if params
                    (return (cons (serialize-param (head params) (head signature)) (serialize-params (tail params) (tail signature))))
                  (return '()))))

(builtin.defn derefw (whathow)
              (progn
                (def what (head whathow))
                (def how (head (tail whathow)))
                (if (eq how 'pointer)
                    (return what)
                  (if (eq how 'fdatum)
                      (return what)
                    (if (eq how 'progslice)
                        (return what)
                      (return (host "deref" whathow)))))))

(builtin.defn pointer-call-and-deserialize (annotated-function-and-params)
              (progn
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
(builtin.defn dlopen (arg) (return (pointer-call-and-deserialize `((~dlopen-pointer ((string) pointer)) (~arg)))))

(def dlsym-pointer (host "dlsym" '()))
(builtin.defn dlsym (arg) (return (pointer-call-and-deserialize `((~dlsym-pointer ((pointer string) pointer)) (~arg)))))

(builtin.defn c-data-pointer (handle c-name signature)
            (progn
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (derefw `(~fn-pointer-pointer int64)))
                (return fn-pointer)))    

(builtin.defn c-function-or-panic (handle c-name signature)
              (progn
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-ptr (derefw `(~fn-pointer-pointer int64)))
                (if (eq fn-ptr 0)
                    (panic "couldn't load C function")
                  (return
                   (builtin.fn (arg)
                    (return (pointer-call-and-deserialize `((~fn-ptr ~signature) (~arg)))))))))
                


(def selflib (dlopen ""))

(builtin.defn builtin-function (c-name signature)
              (progn
                (return (c-function-or-panic selflib c-name signature))))

(def eq (builtin-function "builtin_eq" '((datum datum) val)))
(def annotate (builtin-function "builtin_annotate" '((datum) val)))
(def is-constant (builtin-function "builtin_is_constant" '((datum) val)))
(def repr (builtin-function "builtin_repr" '((datum) val)))
(def concat-bytestrings (builtin-function "builtin_concat_bytestrings" '((datum datum) val)))
(def + (builtin-function "builtin_add" '((datum datum) val)))

(builtin.defn wrap-pointer-into-pointer (x) (return (host "mkptr" `(~x sizet))))


(builtin.defn shared-library (x) (progn
                               (def r (dlopen x))
                               (if (eq 0 (derefw `(~r int64)))
                                   (return `(:err "shared-library failed"))
                                 (return `(:ok ~r)))))

(builtin.defn extern-pointer (handle c-name signature) (progn
                               (def res (c-data-pointer handle c-name signature))
                               (if (eq 0 res)
                                   (return `(:err "extern-pointer failed"))
                                 (return `(:ok ~res)))))
                                        
(builtin.defn debug-print () (return '()))

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
