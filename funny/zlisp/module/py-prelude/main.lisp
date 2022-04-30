(builtin.defn print (return (host ("eval" "print(x[1][0])") args)))

(builtin.defn head (return (host ("eval" "head(x)") args)))

(builtin.defn tail (return (host ("eval" "tail(x)") args)))
