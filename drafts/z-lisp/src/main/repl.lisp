(def last
     (builtin.fn
	 (if (cdr (car args))
	     (last (cdr (car args)))
	   (car (car args)))))
(def progn (builtin.fn (last args)))
(def quote (builtin.form  (car args)))

(def second
     (builtin.fn
      (car (cdr (car args)))))
(def head car)
(def tail cdr)

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

(def fn (builtin.macro `(builtin.fn (progn (decons ~(car args) args) ~(car (cdr args))))))

(def macro (builtin.macro `(builtin.macro (progn (def ~(car args) args) ~(car (cdr args))))))

(def form (builtin.macro `(builtin.form (progn (def ~(car args) args) ~(car (cdr args))))))

(def list (builtin.fn args))

(def defn (macro args `(def ~(car args) ~(cons 'fn (cdr args)))))

(def defmacro (macro args `(def ~(car args) ~(cons 'macro (cdr args)))))

(def defform (macro args `(def ~(car args) ~(cons 'form (cdr args)))))

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
