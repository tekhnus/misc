(def last
     (builtin.fn
	 (if (tail (head args))
	     (last (tail (head args)))
	   (head (head args)))))
(def progn (builtin.fn (last args)))
(def quote (builtin.form  (head args)))

(def second
     (builtin.fn
      (head (tail (head args)))))

(def deconsfn
     (builtin.fn
      (if (head args)
	  `(progn
	     (def ~(head (head args)) (head ~(second args)))
	     ~(deconsfn (tail (head args)) `(tail ~(second args))))
	''())))

(def decons
     (builtin.macro
      (deconsfn (head args) (second args))))

(def fn (builtin.macro `(builtin.fn (progn (decons ~(head args) args) ~(head (tail args))))))

(def macro (builtin.macro `(builtin.macro (progn (def ~(head args) args) ~(head (tail args))))))

(def form (builtin.macro `(builtin.form (progn (def ~(head args) args) ~(head (tail args))))))

(def list (builtin.fn args))

(def defn (macro args `(def ~(head args) ~(cons 'fn (tail args)))))

(def defmacro (macro args `(def ~(head args) ~(cons 'macro (tail args)))))

(def defform (macro args `(def ~(head args) ~(cons 'form (tail args)))))

(defn third args (head (tail (tail (head args)))))

(defn map (f s)
  (if s
      (cons
       (f
	(head s))
       (map
	f
	(tail s)))
    '()))

(defn append (x xs)
  (if xs
      (cons
       (head xs)
       (append
	x
	(tail xs)))
    (list x)))

(def fopen
     (extern-pointer "libSystem.dylib" "fopen"
		((string string) pointer)))
(def malloc
     (extern-pointer "libSystem.dylib" "malloc"
		((sizet) pointer)))
(def fread
     (extern-pointer "libSystem.dylib" "fread"
		  ((pointer sizet sizet pointer) sizet)))

(def feof
     (extern-pointer "libSystem.dylib" "feof"
		((pointer) int)))

(def printfptr
     (extern-pointer "libSystem.dylib" "printf"
		((string pointer) sizet)))

(def fprintfstring
     (extern-pointer "libSystem.dylib" "fprintf"
		((pointer string string) sizet)))

(def stdin
     (extern-pointer "libSystem.dylib" "__stdinp" pointer))

(def stdout
     (extern-pointer "libSystem.dylib" "__stdoutp" pointer))

(def stderr
     (extern-pointer "libSystem.dylib" "__stderrp" pointer))

(defn repl (nsp)
  (progn
    (fprintfstring stdout "%s" "> ")
    (def readres (read stdin))
    (if (tail readres)
	(progn
	  (def datum (second readres))
	  (def v (eval-in nsp datum))
	  (print (second v))
	  (repl nsp))
      (fprintfstring stdout "%s\n" ""))))

(def ns (make-namespace))
(repl ns)
