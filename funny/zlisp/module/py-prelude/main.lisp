(req)

(builtin.defn print (return (host ("eval" "print(x[1][0])") args)))

(builtin.defn debug-print (return (host ("eval" "print(x[1][0])") args)))

(builtin.defn head (return (host ("eval" "head(x)") args)))

(builtin.defn tail (return (host ("eval" "tail(x)") args)))

(builtin.defn is-constant (return (host ("eval" "is_constant(x)") args)))

(builtin.defn eq (return (host ("eval" "eq(x)") args)))

(builtin.defn panic (return (host ("eval" "panic(x)") args)))

(builtin.defn annotate (return (host ("eval" "annotate(x)") args)))

(builtin.defn + (return (host ("eval" "add(x)") args)))

(builtin.defn cons (return (host ("eval" "cons(x)") args)))

(builtin.defn concat-bytestrings (return (host ("eval" "concat_bytestrings(x)") args)))

(builtin.defn repr (return (host ("eval" "repre(x)") args)))

(export)
