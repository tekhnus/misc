(req)

(builtin.defn print (a0) (return (host ("eval" "print(x[1][0])") `(~a0))))

(builtin.defn debug-print (a0) (return (host ("eval" "print(x[1][0])") `(~a0))))

(builtin.defn head (a0) (return (host ("eval" "head(x)") `(~a0))))

(builtin.defn tail (a0) (return (host ("eval" "tail(x)") `(~a0))))

(builtin.defn is-constant (a0) (return (host ("eval" "is_constant(x)") `(~a0))))

(builtin.defn eq (a0 a1) (return (host ("eval" "eq(x)") `(~a0 ~a1))))

(builtin.defn panic (a0) (return (host ("eval" "panic(x)") `(~a0))))

(builtin.defn annotate (a0) (return (host ("eval" "annotate(x)") `(~a0))))

(builtin.defn + (a0 a1) (return (host ("eval" "add(x)") `(~a0 ~a1))))

(builtin.defn cons (a0 a1) (return (host ("eval" "cons(x)") `(~a0 ~a1))))

(builtin.defn concat-bytestrings (a0 a1) (return (host ("eval" "concat_bytestrings(x)") `(~a0 ~a1))))

(builtin.defn repr (a0) (return (host ("eval" "repre(x)") `(~a0))))

(export
 (print print)
 (panic panic)
 (head head)
 (tail tail)
 (cons cons)
 (eq eq)
 (eq eq)
 (annotate annotate)
 (is-constant is-constant)
 (repr repr)
 (concat-bytestrings concat-bytestrings)
 (+ +))
