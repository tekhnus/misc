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

(def type (builtin.fn (head (annotate (head args)))))
(def decons-fn
     (builtin.fn
      (if (is-constant (head args))
	  `(if (eq ~(head args) ~(second args)) :ok :err)
	(if (eq (type (head args)) :symbol)
	    `(progn
	       (def ~(head args) ~(second args))
	       :ok)
	  (if (eq (type (head args)) :list)
	      (if (head args)
		  `(if (eq ~(decons-fn (head (head args)) `(head ~(second args))) :err) :err ~(decons-fn (tail (head args)) `(tail ~(second args))))
		`(if ~(second args) :err :ok))
	    (panic "decons met an unsupported type"))))))

'(print (decons-fn 42 'bar))
'(print (decons-fn :foo 'bar))
'(print (decons-fn 'foo 'bar))
'(print (decons-fn '() 'bar))
'(print (decons-fn '(foo1 foo2) 'bar))

(def decons
     (builtin.macro
      (decons-fn (head args) (second args))))

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
    (panic "cond didn't match")))
(def cond (builtin.macro `(cond- ~args)))

(defn is-nil (val) (if val '() '(())))
(defn switch-decons-fn (exp cases)
  (if (is-nil cases)
      (panic "switch-decons didn't match")
    `(cond- ~(map (fn ((pat val)) `((eq :ok (decons ~pat ~exp)) ~val)) cases))))
(defmacro switch-decons args (switch-decons-fn (head args) (tail args)))

(defmacro handle-error (name) `(switch-decons ~name ((:ok tmp) (def ~name tmp)) ((:err msg) (panic msg))))

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
    (switch-decons readres
		   ((:eof) (fprintfstring stdout "%s\n" ""))
		   ((:ok datum)
		    (progn
		      (def v (eval-in nsp datum))
		      (switch-decons v
				     ((:ok val) (print val))
				     ((:err msg) (fprintfstring stdout "eval error: %s\n" msg)))
		      (repl nsp)))
		   ((:err msg)
		    (progn
		      (fprintfstring stdout "read error: %s\n" msg)
		      (repl nsp))))))

(def ns (make-namespace))
(repl ns)
