(def dlopen-pointer (host "dlopen" '()))
(builtin.defn dlopen (return (host "pointer-call" `(~dlopen-pointer ~args))))

(def dlsym-pointer (host "dlsym" '()))
(builtin.defn dlsym (return (host "pointer-call" `(~dlsym-pointer ~args))))

(def dereference-and-cast-pointer (host "dereference-and-cast" '()))
(builtin.defn dereference-and-cast (return (host "pointer-call" `(~dereference-and-cast-pointer ~args))))

(def not-null-pointer-ptr (host "not-null-pointer" '()))
(builtin.defn not-null-pointer (return (host "pointer-call" `(~not-null-pointer-ptr ~args))))

(def wrap-pointer-into-pointer-ptr (host "wrap-pointer-into-pointer" '()))
(builtin.defn wrap-pointer-into-pointer (return (host "pointer-call" `(~wrap-pointer-into-pointer-ptr ~args))))


(def selflib (dlopen ""))

(def head-pointer-pointer (dlsym selflib "builtin_head"))
(def head-pointer (dereference-and-cast head-pointer-pointer '((datum) val)))
(builtin.defn head (return (host "pointer-call" `(~head-pointer ~args))))

(def tail-pointer-pointer (dlsym selflib "builtin_tail"))
(def tail-pointer (dereference-and-cast tail-pointer-pointer '((datum) val)))
(builtin.defn tail (return (host "pointer-call" `(~tail-pointer ~args))))

(builtin.defn c-function-pointer
            (progn
              (def handle (head args))
                (def c-name (head (tail args)))
                (def signature (head (tail (tail args))))
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (dereference-and-cast fn-pointer-pointer signature))
                (return fn-pointer)))
                
(builtin.defn c-function
              (progn
                (def handle (head args))
                (def c-name (head (tail args)))
                (def signature (head (tail (tail args))))
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (dereference-and-cast fn-pointer-pointer signature))
                (def fn-routine (builtin.fn (return (host "pointer-call" `(~fn-pointer ~args)))))
                (return fn-routine)))
              
(builtin.defn builtin-function
              (progn
                (def c-name (head args))
                (def signature (head (tail args)))
                (return (c-function selflib c-name signature))))

(def cons (builtin-function "builtin_cons" '((datum datum) val)))
(def panic (builtin-function "builtin_panic" '((datum) val)))
(def eq (builtin-function "builtin_eq" '((datum datum) val)))
(def annotate (builtin-function "builtin_annotate" '((datum) val)))
(def is-constant (builtin-function "builtin_is_constant" '((datum) val)))
(def repr (builtin-function "builtin_repr" '((datum) val)))
(def concat-bytestrings (builtin-function "builtin_concat_bytestrings" '((datum datum) val)))
(def + (builtin-function "builtin_add" '((datum datum) val)))

(builtin.defn dlopen-or-panic (progn
                               (def res-ptr (dlopen (head args)))
                               (def res (dereference-and-cast res-ptr 'pointer))
                               (if (not-null-pointer res)
                                   (return res-ptr)
                                 (panic (concat-bytestrings "couln't dlopen library " (head args))))))

(builtin.defn c-function-or-panic
              (progn
                (def handle (head args))
                (def c-name (head (tail args)))
                (def signature (head (tail (tail args))))
                (def fn-pointer-pointer (dlsym handle c-name))
                (def fn-pointer (dereference-and-cast fn-pointer-pointer signature))
                (if (not-null-pointer fn-pointer)
                    ((def fn-routine (builtin.fn (return (host "pointer-call" `(~fn-pointer ~args)))))
                     (return fn-routine))
                  (panic (concat-bytestrings "couldn't load C function " c-name)))))
                  

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

(builtin.defn wrap-fn-pointer (return `(def ~(head args) (builtin.fn (return (host "pointer-call" (list ~(head (tail args)) args)))))))
