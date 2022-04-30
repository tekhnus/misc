(builtin.defn print (return (host ("eval" "print(x[1][0])") args)))

(builtin.defn debug-print (return (host ("eval" "print(x[1][0])") args)))

(builtin.defn head (return (host ("eval" "head(x)") args)))

(builtin.defn tail (return (host ("eval" "tail(x)") args)))

(builtin.defn is-constant (return (host ("eval" "is_constant(x)") args)))

(builtin.defn eq (return (host ("eval" "eq(x)") args)))

(builtin.defn panic (return (host ("eval" "panic(x)") args)))
