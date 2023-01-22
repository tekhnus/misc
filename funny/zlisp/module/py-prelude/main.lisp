(req)

(defn print (a0) (return (return @(host ("eval" "print(x[1][0])")) `(~a0))))

(defn debug-print (a0) (return (return @(host ("eval" "print(x[1][0])")) `(~a0))))

(defn head (a0) (return (return @(host ("eval" "head(x)")) `(~a0))))

(defn tail (a0) (return (return @(host ("eval" "tail(x)")) `(~a0))))

(defn is-constant (a0) (return (return @(host ("eval" "is_constant(x)")) `(~a0))))

(defn eq (a0 a1) (return (return @(host ("eval" "eq(x)")) `(~a0 ~a1))))

(defn panic (a0) (return (return @(host ("eval" "panic(x)")) `(~a0))))

(defn annotate (a0) (return (return @(host ("eval" "annotate(x)")) `(~a0))))

(defn + (a0 a1) (return (return @(host ("eval" "add(x)")) `(~a0 ~a1))))

(defn cons (a0 a1) (return (return @(host ("eval" "cons(x)")) `(~a0 ~a1))))

(defn concat-bytestrings (a0 a1) (return (return @(host ("eval" "concat_bytestrings(x)")) `(~a0 ~a1))))

(defn repr (a0) (return (return @(host ("eval" "repre(x)")) `(~a0))))

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
