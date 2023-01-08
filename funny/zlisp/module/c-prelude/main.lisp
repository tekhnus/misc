(req)

(def deref-pointer (host "deref-pointer" '()))
(defn deref (x y) (return (host "call-extension" deref-pointer x y)))

(def mkptr-pointer (host "mkptr-pointer" '()))
(defn mkptr (x y) (return (host "call-extension" mkptr-pointer x y)))

(def pointer-call-pointer (host "pointer-call-pointer" '()))
(defn pointer-call (x y z) (return (host "call-extension" pointer-call-pointer x y z)))

(def panic-pointer (host "panic" '()))
(defn panic (x) (return (host "call-extension" panic-pointer x)))

(def head-pointer (host "head" '()))
(defn head (x) (return (host "call-extension" head-pointer x)))

(def tail-pointer (host "tail" '()))
(defn tail (x) (return (host "call-extension" tail-pointer x)))

(def cons-pointer (host "cons" '()))
(defn cons (x xs) (return (host "call-extension" cons-pointer x xs)))

(def eq-pointer (host "eq" '()))
(defn eq (x y) (return (host "call-extension" eq-pointer x y)))

(defn serialize-param (param signature)
  (progn
    (if (eq signature 'pointer)
        (return param)
      (if (eq signature 'fdatum)
          (return param)
        (if (eq signature 'progslice)
            (return param)
          (return (mkptr param signature)))))))

(defn serialize-params (params signature)
  (progn
    (if params
        (return (cons (serialize-param (head params) (head signature)) (serialize-params (tail params) (tail signature))))
      (return '()))))

(defn  derefw2 (what how)
  (progn
    (if (eq how 'pointer)
        (return what)
      (if (eq how 'fdatum)
          (return what)
        (if (eq how 'progslice)
            (return what)
          (return (deref what how)))))))

(defn pointer-call-and-deserialize (fn-ptr signature params)
  (progn
    (def fnparamst (head signature))
    (def rettype (head (tail signature)))
    (def s (serialize-params params fnparamst))
    (def rawres (pointer-call fn-ptr `(~fnparamst ~rettype) s))
    (return (derefw2 rawres rettype))))

(def rtld-lazy (host "RTLD_LAZY" '()))

(def dlopen-pointer (host "dlopen" '()))
"TODO: dlopen actually has an int argument, not a size_t."
(defn dlopen (x) (return (pointer-call-and-deserialize dlopen-pointer '((string sizet) pointer) `(~x ~rtld-lazy))))
(defn dlopen-null () (return (pointer-call-and-deserialize dlopen-pointer '((pointer sizet) pointer) `(~(mkptr 0 'sizet) ~rtld-lazy))))

(def dlsym-pointer (host "dlsym" '()))
(defn dlsym (x y) (return (pointer-call-and-deserialize dlsym-pointer '((pointer string) pointer) `(~x ~y))))

(defn c-data-pointer (handle c-name signature)
  (progn
    (def fn-pointer-pointer (dlsym handle c-name))
    (def fn-pointer (derefw2 fn-pointer-pointer 'int64))
    (return fn-pointer)))    

(defn nth (n xs)
  (progn
    (if xs
        (if n
            (return (nth (tail n) (tail xs)))
          (return (head xs)))
      (panic "nth fail"))))

(defn get-fn-ptr (handle c-name)
  (progn
    (def fn-pointer-pointer (dlsym handle c-name))
    (def fn-ptr (derefw2 fn-pointer-pointer 'int64))
    (if (eq fn-ptr 0)
        (panic "couldn't load C function")
      (return fn-ptr))))
(defn c-function-or-panic-new-0 (fn-ptr signature)
  (progn
    (def () (return @0 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `()))))
(defn c-function-or-panic-new-1 (fn-ptr signature)
  (progn
    (def (a1) (return @1 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1)))))
(defn c-function-or-panic-new-2 (fn-ptr signature)
  (progn
    (def (a1 a2) (return @2 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2)))))
(defn c-function-or-panic-new-3 (fn-ptr signature)
  (progn
    (def (a1 a2 a3) (return @3 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2 ~a3)))))
(defn c-function-or-panic-new-4 (fn-ptr signature)
  (progn
    (def (a1 a2 a3 a4) (return @4 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2 ~a3 ~a4)))))
(defn c-function-or-panic-new-5 (fn-ptr signature)
  (progn
    (def (a1 a2 a3 a4 a5) (return @5 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2 ~a3 ~a4 ~a5)))))

(defn c-function-or-panic-new-6 (fn-ptr signature)
  (progn
    (def (a1 a2 a3 a4 a5 a6) (return @6 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2 ~a3 ~a4 ~a5 ~a6)))))

(defn c-function-or-panic-new-7 (fn-ptr signature)
  (progn
    (def (a1 a2 a3 a4 a5 a6 a7) (return @7 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2 ~a3 ~a4 ~a5 ~a6 ~a7)))))

(defn c-function-or-panic-new (handle c-name signature)
  (progn
    (def argssig (head signature))
    (def objs `( ~c-function-or-panic-new-0 ~c-function-or-panic-new-1 ~c-function-or-panic-new-2 ~c-function-or-panic-new-3 ~c-function-or-panic-new-4 ~c-function-or-panic-new-5 ~c-function-or-panic-new-6 ~c-function-or-panic-new-7))
    (def obj (nth argssig objs))
    (def fn-ptr (get-fn-ptr handle c-name))
    (@obj fn-ptr signature)
    (return obj)))

(def selflib (dlopen-null))

(def annotate-pointer (derefw2 (dlsym selflib "builtin_annotate") 'int64))
(defn annotate (x) (return (host "call-extension" annotate-pointer x)))

(def is-constant-pointer (derefw2 (dlsym selflib "builtin_is_constant") 'int64))
(defn is-constant (x) (return (host "call-extension" is-constant-pointer x)))

(def repr-pointer (derefw2 (dlsym selflib "builtin_repr") 'int64))
(defn repr (x) (return (host "call-extension" repr-pointer x)))

(def concat-bytestrings-pointer (derefw2 (dlsym selflib "builtin_concat_bytestrings") 'int64))
(defn concat-bytestrings (x y) (return (host "call-extension" concat-bytestrings-pointer x y)))

(def +-pointer (derefw2 (dlsym selflib "builtin_add") 'int64))
(defn + (x y) (return (host "call-extension" +-pointer x y)))

(defn wrap-pointer-into-pointer (p) (return (mkptr p 'sizet)))


(defn shared-library (path) (progn
                              (def r (dlopen path))
                              (if (eq 0 (derefw2 r 'int64))
                                  (return `(:err "shared-library failed"))
                                (return `(:ok ~r)))))

(defn extern-pointer (handle c-name signature) (progn
                                                 (def res (c-data-pointer handle c-name signature))
                                                 (if (eq 0 res)
                                                     (return `(:err "extern-pointer failed"))
                                                   (return `(:ok ~res)))))

(export
 (panic panic)
 (head head)
 (tail tail)
 (cons cons)
 (eq eq)
 (derefw2 derefw2)
 (dlsym dlsym)
 (c-function-or-panic-new c-function-or-panic-new)
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
