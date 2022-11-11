(req)

(def deref-pointer (host "deref-pointer" '()))
(builtin.defn deref (x y) (return (host "call-extension" deref-pointer x y)))

(def mkptr-pointer (host "mkptr-pointer" '()))
(builtin.defn mkptr (x y) (return (host "call-extension" mkptr-pointer x y)))

(def pointer-call-pointer (host "pointer-call-pointer" '()))
(builtin.defn pointer-call (x y z) (return (host "call-extension" pointer-call-pointer x y z)))

(def panic-pointer (host "panic" '()))
(builtin.defn panic (x) (return (host "call-extension" panic-pointer x)))

(def head-pointer (host "head" '()))
(builtin.defn head (x) (return (host "call-extension" head-pointer x)))

(def tail-pointer (host "tail" '()))
(builtin.defn tail (x) (return (host "call-extension" tail-pointer x)))

(def cons-pointer (host "cons" '()))
(builtin.defn cons (x xs) (return (host "call-extension" cons-pointer x xs)))

(def eq-pointer (host "eq" '()))
(builtin.defn eq (x y) (return (host "call-extension" eq-pointer x y)))

(builtin.defn serialize-param (param signature)
              (progn
                (if (eq signature 'pointer)
                    (return param)
                  (if (eq signature 'fdatum)
                      (return param)
                    (if (eq signature 'progslice)
                        (return param)
                      (return (mkptr param signature)))))))

(builtin.defn serialize-params (params signature)
              (progn
                (if params
                    (return (cons (serialize-param (head params) (head signature)) ((resolve serialize-params) (tail params) (tail signature))))
                  (return '()))))

(builtin.defn  derefw2 (what how)
              (progn
                (if (eq how 'pointer)
                    (return what)
                  (if (eq how 'fdatum)
                      (return what)
                    (if (eq how 'progslice)
                        (return what)
                      (return (deref what how)))))))

(builtin.defn pointer-call-and-deserialize (fn-ptr signature params)
              (progn
                (def fnparamst (head signature))
                (def rettype (head (tail signature)))
                (def s (serialize-params params fnparamst))
                (def rawres (pointer-call fn-ptr `(~fnparamst ~rettype) s))
                (return (derefw2 rawres rettype))))

(def rtld-lazy (host "RTLD_LAZY" '()))

(def dlopen-pointer (host "dlopen" '()))
"TODO: dlopen actually has an int argument, not a size_t."
(builtin.defn dlopen (x) (return (pointer-call-and-deserialize dlopen-pointer '((string sizet) pointer) `(~x ~rtld-lazy))))
(builtin.defn dlopen-null () (return (pointer-call-and-deserialize dlopen-pointer '((pointer sizet) pointer) `(~(mkptr 0 'sizet) ~rtld-lazy))))

(def dlsym-pointer (host "dlsym" '()))
(builtin.defn dlsym (x y) (return (pointer-call-and-deserialize dlsym-pointer '((pointer string) pointer) `(~x ~y))))

(builtin.defn c-data-pointer (handle c-name signature)
            (progn
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (derefw2 fn-pointer-pointer 'int64))
                (return fn-pointer)))    

(builtin.defn nth (n xs)
              (progn
                (if xs
                    (if n
                        (return ((resolve nth) (tail n) (tail xs)))
                      (return (head xs)))
                  (panic "nth fail"))))

(def get-pcads (builtin.fn (fn-ptr signature) (progn
(def pointer-call-and-deserialize-0 (builtin.fn () (return (pointer-call-and-deserialize fn-ptr signature `()))))
(def pointer-call-and-deserialize-1 (builtin.fn (arg1) (return (pointer-call-and-deserialize fn-ptr signature `(~arg1)))))
(def pointer-call-and-deserialize-2 (builtin.fn (arg1 arg2) (return (pointer-call-and-deserialize fn-ptr signature `(~arg1 ~arg2)))))
(def pointer-call-and-deserialize-3 (builtin.fn (arg1 arg2 a3) (return (pointer-call-and-deserialize fn-ptr signature `(~arg1 ~arg2 ~a3)))))
(def pointer-call-and-deserialize-4 (builtin.fn (arg1 arg2 a3 a4) (return (pointer-call-and-deserialize fn-ptr signature `(~arg1  ~arg2 ~a3 ~a4)))))
(def pointer-call-and-deserialize-5 (builtin.fn (arg1 arg2 a3 a4 a5) (return (pointer-call-and-deserialize fn-ptr signature `(~arg1 ~arg2 ~a3 ~a4 ~a5)))))
(def pointer-call-and-deserialize-6 (builtin.fn (arg1 arg2 a3 a4 a5 a6) (return (pointer-call-and-deserialize fn-ptr signature `(~arg1 ~arg2 ~a3 ~a4 ~a5 ~a6)))))
(def pointer-call-and-deserialize-7 (builtin.fn (arg1 arg2 a3 a4 a5 a6 a7) (return (pointer-call-and-deserialize fn-ptr signature `(~arg1 ~arg2 ~a3 ~a4 ~a5 ~a6 ~a7)))))
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
                (def fn-ptr (derefw2 fn-pointer-pointer 'int64))
                (if (eq fn-ptr 0)
                    (panic "couldn't load C function")
                  (progn
                    (def pcadfns (get-pcads fn-ptr signature))
                    (def pcadfn (nth argssig pcadfns))
                    (return pcadfn)))))

(def selflib (dlopen-null))

(def annotate-pointer (derefw2 (dlsym selflib "builtin_annotate") 'int64))
(builtin.defn annotate (x) (return (host "call-extension" annotate-pointer x)))

(def is-constant-pointer (derefw2 (dlsym selflib "builtin_is_constant") 'int64))
(builtin.defn is-constant (x) (return (host "call-extension" is-constant-pointer x)))

(def repr-pointer (derefw2 (dlsym selflib "builtin_repr") 'int64))
(builtin.defn repr (x) (return (host "call-extension" repr-pointer x)))

(def concat-bytestrings-pointer (derefw2 (dlsym selflib "builtin_concat_bytestrings") 'int64))
(builtin.defn concat-bytestrings (x y) (return (host "call-extension" concat-bytestrings-pointer x y)))

(def +-pointer (derefw2 (dlsym selflib "builtin_add") 'int64))
(builtin.defn + (x y) (return (host "call-extension" +-pointer x y)))

(builtin.defn wrap-pointer-into-pointer (p) (return (mkptr p 'sizet)))


(builtin.defn shared-library (path) (progn
                               (def r (dlopen path))
                               (if (eq 0 (derefw2 r 'int64))
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
 (derefw2 derefw2)
 (mkptr mkptr)
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
