(req)

(def panic-pointer (host "panic" '()))
(builtin.defn panic (x) (return (host "deref" (host "pointer-call-datums" panic-pointer  '((datum) val)  `(~x)) 'val)))

(def head-pointer (host "head" '()))
(builtin.defn head (x) (return (host "deref" (host "pointer-call-datums" head-pointer  '((datum) val)  `(~x)) 'val)))

(def tail-pointer (host "tail" '()))
(builtin.defn tail (x) (return (host "deref" (host "pointer-call-datums" tail-pointer  '((datum) val)  `(~x)) 'val)))

(def cons-pointer (host "cons" '()))
(builtin.defn cons (x xs) (return (host "deref" (host "pointer-call-datums" cons-pointer  '((datum datum) val)  `(~x ~xs)) 'val)))

(def eq-pointer (host "eq" '()))
(builtin.defn eq (x y) (return (host "deref" (host "pointer-call-datums" eq-pointer  '((datum datum) val)  `(~x ~y)) 'val)))

(builtin.defn serialize-param (param signature)
              (progn
                (if (eq signature 'pointer)
                    (return param)
                  (if (eq signature 'fdatum)
                      (return param)
                    (if (eq signature 'progslice)
                        (return param)
                      (return (host "mkptr" param signature)))))))

(builtin.defn serialize-params (params signature)
              (progn
                (if params
                    (return (cons (serialize-param (head params) (head signature)) ((resolve serialize-params) (tail params) (tail signature))))
                  (return '()))))

(builtin.defn  derefw (whathow)
              (progn
                (def what (head whathow))
                (def how (head (tail whathow)))
                (if (eq how 'pointer)
                    (return what)
                  (if (eq how 'fdatum)
                      (return what)
                    (if (eq how 'progslice)
                        (return what)
                      (return (host "deref" what how)))))))

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
                (def rawres (host "pointer-call" fn-ptr `(~fnparamst ~rettype) s))
                (return (derefw `(~rawres ~rettype)))))


(def dlopen-pointer (host "dlopen" '()))
(builtin.defn dlopen (x) (return (pointer-call-and-deserialize `((~dlopen-pointer ((string) pointer)) (~x)))))

(def dlsym-pointer (host "dlsym" '()))
(builtin.defn dlsym (x y) (return (pointer-call-and-deserialize `((~dlsym-pointer ((pointer string) pointer)) (~x ~y)))))

(builtin.defn c-data-pointer (handle c-name signature)
            (progn
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (derefw `(~fn-pointer-pointer int64)))
                (return fn-pointer)))    

(builtin.defn nth (n xs)
              (progn
                (if xs
                    (if n
                        (return ((resolve nth) (tail n) (tail xs)))
                      (return (head xs)))
                  (panic "nth fail"))))

(def get-pcads (builtin.fn (fn-ptr signature) (progn
(def pointer-call-and-deserialize-0 (builtin.fn () (return (pointer-call-and-deserialize `((~fn-ptr ~signature) ())))))
(def pointer-call-and-deserialize-1 (builtin.fn (arg1) (return (pointer-call-and-deserialize `((~fn-ptr ~signature) (~arg1))))))
(def pointer-call-and-deserialize-2 (builtin.fn (arg1 arg2) (return (pointer-call-and-deserialize `((~fn-ptr ~signature) (~arg1 ~arg2))))))
(def pointer-call-and-deserialize-3 (builtin.fn (arg1 arg2 a3) (return (pointer-call-and-deserialize `((~fn-ptr ~signature) (~arg1 ~arg2 ~a3))))))
(def pointer-call-and-deserialize-4 (builtin.fn (arg1 arg2 a3 a4) (return (pointer-call-and-deserialize `((~fn-ptr ~signature) (~arg1  ~arg2 ~a3 ~a4))))))
(def pointer-call-and-deserialize-5 (builtin.fn (arg1 arg2 a3 a4 a5) (return (pointer-call-and-deserialize `((~fn-ptr ~signature) (~arg1 ~arg2 ~a3 ~a4 ~a5))))))
(def pointer-call-and-deserialize-6 (builtin.fn (arg1 arg2 a3 a4 a5 a6) (return (pointer-call-and-deserialize `((~fn-ptr ~signature) (~arg1 ~arg2 ~a3 ~a4 ~a5 ~a6))))))
(def pointer-call-and-deserialize-7 (builtin.fn (arg1 arg2 a3 a4 a5 a6 a7) (return (pointer-call-and-deserialize `((~fn-ptr ~signature) (~arg1 ~arg2 ~a3 ~a4 ~a5 ~a6 ~a7))))))
(return `(~pointer-call-and-deserialize-0
             ~pointer-call-and-deserialize-1
             ~pointer-call-and-deserialize-2
             ~pointer-call-and-deserialize-3
             ~pointer-call-and-deserialize-4
             ~pointer-call-and-deserialize-5
             ~pointer-call-and-deserialize-6
             ~pointer-call-and-deserialize-7)))))

(builtin.defn c-function-or-panic (handle c-name signature)
              (progn
                (def argssig (head signature))
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-ptr (derefw `(~fn-pointer-pointer int64)))
                (if (eq fn-ptr 0)
                    (panic "couldn't load C function")
                  (progn
                    (def pcadfns (get-pcads fn-ptr signature))
                    (def pcadfn (nth argssig pcadfns))
                    (return pcadfn)))))

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

(builtin.defn wrap-pointer-into-pointer (p) (return (host "mkptr" p 'sizet)))


(builtin.defn shared-library (path) (progn
                               (def r (dlopen path))
                               (if (eq 0 (derefw `(~r int64)))
                                   (return `(:err "shared-library failed"))
                                 (return `(:ok ~r)))))

(builtin.defn extern-pointer (handle c-name signature) (progn
                               (def res (c-data-pointer handle c-name signature))
                               (if (eq 0 res)
                                   (return `(:err "extern-pointer failed"))
                                 (return `(:ok ~res)))))
                                        
(builtin.defn debug-print (x) (return '()))

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
