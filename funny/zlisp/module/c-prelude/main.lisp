(def dlopen-pointer (builtin-pointer "dlopen"))
(builtin.defn dlopen (return (pointer-call dlopen-pointer args)))

(def dlsym-pointer (builtin-pointer "dlsym"))
(builtin.defn dlsym (return (pointer-call dlsym-pointer args)))

(def dereference-and-cast-pointer (builtin-pointer "dereference-and-cast"))
(builtin.defn dereference-and-cast (return (pointer-call dereference-and-cast-pointer args)))

(def not-null-pointer-ptr (builtin-pointer "not-null-pointer"))
(builtin.defn not-null-pointer (return (pointer-call not-null-pointer-ptr args)))

(def selflib (dlopen ""))

(def head-pointer-pointer (dlsym selflib "builtin_head"))
(def head-pointer (dereference-and-cast head-pointer-pointer '((datum) val)))
(builtin.defn head (return (pointer-call head-pointer args)))

(def tail-pointer-pointer (dlsym selflib "builtin_tail"))
(def tail-pointer (dereference-and-cast tail-pointer-pointer '((datum) val)))
(builtin.defn tail (return (pointer-call tail-pointer args)))

(builtin.defn builtin-function
              (progn
                (def c-name (head args))
                (def signature (head (tail args)))
                (def fn-pointer-pointer (dlsym selflib c-name))
                (def fn-pointer (dereference-and-cast fn-pointer-pointer signature))
                (def fn-routine (builtin.fn (return (pointer-call fn-pointer args))))
                (return fn-routine)))

(def cons (builtin-function "builtin_cons" '((datum datum) val)))
(def panic (builtin-function "builtin_panic" '((datum) val)))
(def eq (builtin-function "builtin_eq" '((datum datum) val)))
(def annotate (builtin-function "builtin_annotate" '((datum) val)))
(def is-constant (builtin-function "builtin_is_constant" '((datum) val)))
(def repr (builtin-function "builtin_repr" '((datum) val)))
(def concat-bytestrings (builtin-function "builtin_concat_bytestrings" '((datum datum) val)))
(def + (builtin-function "builtin_add" '((datum datum) val)))

(def lowlevel-extern-pointer-- (builtin-pointer "lowlevel-extern-pointer"))

(builtin.defn shared-library (progn
                               (def res-ptr (dlopen (head args)))
                               (def res (dereference-and-cast res-ptr 'pointer))
                               (if (not-null-pointer res)
                                   (return `(:ok ~res-ptr))
                                 (return `(:err "shared-library failed")))))

(builtin.defn extern-pointer (progn
                               (def res (pointer-call lowlevel-extern-pointer-- args))
                               (if (not-null-pointer res)
                                   (return `(:ok ~res))
                                 (return `(:err "extern-pointer failed")))))
