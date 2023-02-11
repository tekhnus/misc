(req)

(defn call-extension-1 (fnptr x) (return (return @(host "call-extension") fnptr x)))

(def deref-pointer (return @(host "deref-pointer") '()))
(defn deref (x y) (return (return @(host "call-extension") deref-pointer x y)))

(def mkptr-pointer (return @(host "mkptr-pointer") '()))
(defn mkptr (x y) (return (return @(host "call-extension") mkptr-pointer x y)))

(def pointer-call-pointer (return @(host "pointer-call-pointer") '()))
(defn pointer-call (x y z) (return (return @(host "call-extension") pointer-call-pointer x y z)))

(def panic-pointer (return @(host "panic") '()))
(defn panic (x) (return (return @(host "call-extension") panic-pointer x)))

(def head-pointer (return @(host "head") '()))
(defn head (x) (return (return @(host "call-extension-1") head-pointer x)))

(def tail-pointer (return @(host "tail") '()))
(defn tail (x) (return (return @(host "call-extension-1") tail-pointer x)))

(def cons-pointer (return @(host "cons") '()))
(defn cons (x xs) (return (return @(host "call-extension") cons-pointer x xs)))

(def eq-pointer (return @(host "eq") '()))
(defn eq (x y) (return (return @(host "call-extension") eq-pointer x y)))

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

(defn  dereference (what how)
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
    (return (dereference rawres rettype))))

(def rtld-lazy (return @(host "RTLD_LAZY") '()))

(def dlopen-pointer (return @(host "dlopen") '()))
"TODO: dlopen actually has an int argument, not a size_t."
(defn dlopen (x) (return (pointer-call-and-deserialize dlopen-pointer '((string sizet) pointer) `(~x ~rtld-lazy))))
(defn dlopen-null () (return (../pointer-call-and-deserialize dlopen-pointer '((pointer sizet) pointer) `(~(../mkptr 0 'sizet) ~rtld-lazy))))

(def dlsym-pointer (return @(host "dlsym") '()))
(defn dlsym (x y) (return (pointer-call-and-deserialize dlsym-pointer '((pointer string) pointer) `(~x ~y))))

(defn c-data-pointer (handle c-name signature)
  (progn
    (def fn-pointer-pointer (dlsym handle c-name))
    (def fn-pointer (dereference fn-pointer-pointer 'int64))
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
    (def fn-ptr (dereference fn-pointer-pointer 'int64))
    (if (eq fn-ptr 0)
        (panic "couldn't load C function")
      (return fn-ptr))))
(defn c-function-0 (fn-ptr signature)
  (progn
    (def () (return @0 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `()))))
(defn c-function-1 (fn-ptr signature)
  (progn
    (def (a1) (return @1 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1)))))
(defn c-function-2 (fn-ptr signature)
  (progn
    (def (a1 a2) (return @2 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2)))))
(defn c-function-3 (fn-ptr signature)
  (progn
    (def (a1 a2 a3) (return @3 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2 ~a3)))))
(defn c-function-4 (fn-ptr signature)
  (progn
    (def (a1 a2 a3 a4) (return @4 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2 ~a3 ~a4)))))
(defn c-function-5 (fn-ptr signature)
  (progn
    (def (a1 a2 a3 a4 a5) (return @5 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2 ~a3 ~a4 ~a5)))))

(defn c-function-6 (fn-ptr signature)
  (progn
    (def (a1 a2 a3 a4 a5 a6) (return @6 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2 ~a3 ~a4 ~a5 ~a6)))))

(defn c-function-7 (fn-ptr signature)
  (progn
    (def (a1 a2 a3 a4 a5 a6 a7) (return @7 :ready))
    (return (pointer-call-and-deserialize fn-ptr signature `(~a1 ~a2 ~a3 ~a4 ~a5 ~a6 ~a7)))))

(defn c-function (handle c-name signature)
  (progn
    (def argssig (head signature))
    (def objs `( ~c-function-0 ~c-function-1 ~c-function-2 ~c-function-3 ~c-function-4 ~c-function-5 ~c-function-6 ~c-function-7))
    (def obj (nth argssig objs))
    (def fn-ptr (get-fn-ptr handle c-name))
    (obj @mut fn-ptr signature)
    (return obj)))

(def selflib (dlopen-null))

(def annotate-pointer (dereference (dlsym selflib "builtin_annotate") 'int64))
(defn annotate (x) (return (return @(host "call-extension") annotate-pointer x)))

(def is-constant-pointer (dereference (dlsym selflib "builtin_is_constant") 'int64))
(defn is-constant (x) (return (return @(host "call-extension") is-constant-pointer x)))

(def repr-pointer (dereference (dlsym selflib "builtin_repr") 'int64))
(defn repr (x) (return (return @(host "call-extension") repr-pointer x)))

(def concat-bytestrings-pointer (dereference (dlsym selflib "builtin_concat_bytestrings") 'int64))
(defn concat-bytestrings (x y) (return (return @(host "call-extension") concat-bytestrings-pointer x y)))

(def +-pointer (dereference (dlsym selflib "builtin_add") 'int64))
(defn + (x y) (return (return @(host "call-extension") +-pointer x y)))

(defn wrap-pointer-into-pointer (p) (return (mkptr p 'sizet)))


(defn shared-library (path) (progn
                              (def r (dlopen path))
                              (if (eq 0 (dereference r 'int64))
                                  (return `(:err "shared-library failed"))
                                (return `(:ok ~r)))))

(defn extern-pointer (handle c-name signature) (progn
                                                 (def res (c-data-pointer handle c-name signature))
                                                 (if (eq 0 res)
                                                     (return `(:err "extern-pointer failed"))
                                                   (return `(:ok ~res)))))

(export
 (call-extension-1 call-extension-1)
 (panic panic)
 (head head)
 (tail tail)
 (cons cons)
 (eq eq)
 (dereference dereference)
 (dlsym dlsym)
 (c-function c-function)
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
