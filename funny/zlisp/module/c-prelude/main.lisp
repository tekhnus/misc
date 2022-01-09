(def lowlevel-shared-library-- (builtin-pointer "lowlevel-shared-library"))
(builtin.defn lowlevel-shared-library (return (pointer-call lowlevel-shared-library--
                                                            args)))
(def lowlevel-extern-pointer-- (builtin-pointer "lowlevel-extern-pointer"))
(builtin.defn lowlevel-extern-pointer (return (pointer-call lowlevel-extern-pointer--
                                                            args)))

(def selflib (lowlevel-shared-library ""))

(def panic-- (lowlevel-extern-pointer selflib "builtin_panic" '((datum) val)))
(def cons-- (lowlevel-extern-pointer selflib "builtin_cons" '((datum datum) val)))
(def head-- (lowlevel-extern-pointer selflib "builtin_head" '((datum) val)))
(def tail-- (lowlevel-extern-pointer selflib "builtin_tail" '((datum) val)))
(def eq-- (lowlevel-extern-pointer selflib "builtin_eq" '((datum datum) val)))
(def annotate-- (lowlevel-extern-pointer selflib "builtin_annotate" '((datum) val)))
(def is-constant-- (lowlevel-extern-pointer selflib "builtin_is_constant" '((datum) val)))
(def repr-- (lowlevel-extern-pointer selflib "builtin_repr" '((datum) val)))
(def concat-bytestrings-- (lowlevel-extern-pointer selflib "builtin_concat_bytestrings" '((datum datum) val)))
(def +-- (lowlevel-extern-pointer selflib "builtin_add" '((datum datum) val)))
(builtin.defn panic (return (pointer-call panic-- args)))
(builtin.defn cons (return (pointer-call cons-- args)))
(builtin.defn head (return (pointer-call head-- args)))
(builtin.defn tail (return (pointer-call tail-- args)))
(builtin.defn eq (return (pointer-call eq-- args)))
(builtin.defn annotate (return (pointer-call annotate-- args)))
(builtin.defn is-constant (return (pointer-call is-constant-- args)))
(builtin.defn repr (return (pointer-call repr-- args)))
(builtin.defn concat-bytestrings (return (pointer-call
                                          concat-bytestrings-- args)))
(builtin.defn + (return (pointer-call +-- args)))

(def shared-library-- (builtin-pointer "shared-library"))
(builtin.defn shared-library (return (pointer-call shared-library--
                                                   args)))

(def extern-pointer-- (builtin-pointer "extern-pointer"))
(builtin.defn extern-pointer (return (pointer-call extern-pointer--
                                                   args)))
