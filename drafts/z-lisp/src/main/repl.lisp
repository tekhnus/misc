(def last
     (builtin.fn
	 (if (tail (head args))
	     (last (tail (head args)))
	   (head (head args)))))
(def progn (builtin.fn (last args)))
(def quote (builtin.operator  (head args)))

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

(def macro (builtin.macro `(builtin.macro (progn (decons ~(head args) args) ~(head (tail args))))))

(def form (builtin.macro `(builtin.operator (progn (decons ~(head args) args) ~(head (tail args))))))

(def list (builtin.fn args))

(def defn (builtin.macro `(def ~(head args) ~(cons 'fn (tail args)))))

(def defmacro (builtin.macro `(def ~(head args) ~(cons 'macro (tail args)))))

(def defform (builtin.macro `(def ~(head args) ~(cons 'form (tail args)))))

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

(defmacro cond- (cases)
  (if cases
      `(if ~(head (head cases))
	 ~(second (head cases))
	 (cond- ~(tail cases)))
    ''()))
(def cond (builtin.macro `(cond- ~args)))

(defmacro handle-error (name) `(def ~name (second ~name)))

(def libc (load-shared-library "libc.so.6"))
(handle-error libc)
	   
(def fopen
     (extern-pointer libc "fopen"
		     ((string string) pointer)))
(handle-error fopen)

(def malloc
     (extern-pointer libc "malloc"
		     ((sizet) pointer)))
(handle-error malloc)

(def fread
     (extern-pointer libc "fread"
		     ((pointer sizet sizet pointer) sizet)))
(handle-error fread)

(def feof
     (extern-pointer libc "feof"
		     ((pointer) int)))
(handle-error feof)

(def printfptr
     (extern-pointer libc "printf"
		     ((string pointer) sizet)))
(handle-error printfptr)

(def fprintfstring
     (extern-pointer libc "fprintf"
		     ((pointer string string) sizet)))
(handle-error fprintfstring)

(def stdin
     (extern-pointer libc "stdin" pointer))
(handle-error stdin)

(def stdout
     (extern-pointer libc "stdout" pointer))
(handle-error stdout)

(def stderr
     (extern-pointer libc "stderr" pointer))
(handle-error stderr)

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
