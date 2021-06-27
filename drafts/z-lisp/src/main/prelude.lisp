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

(def decons
     (builtin.macro
      (decons-fn (head args) (second args))))


(def code-block
     (builtin.macro
      `(if (eq :err (decons ~(head args) args))
	   (list :continue)
	 (list :break ~(cons 'progn (tail args))))))

(def map (builtin.fn
  (if (head (tail args))
      (cons
       ((head args)
	(head (head (tail args))))
       (map
	(head args)
	(tail (head (tail args)))))
    '())))

(def switch-blocks (builtin.macro (cons 'builtin.switch (map (builtin.fn (cons 'code-block (head args))) args))))

(def panic-block '(argz (panic "wrong fn call")))

(def progn- (builtin.fn (cons 'progn (head args))))

(def fn (builtin.macro `(builtin.fn (switch-blocks ~args ~panic-block))))

(def macro (builtin.macro `(builtin.macro (switch-blocks ~args ~panic-block))))

(def list (builtin.fn args))

(def defn (builtin.macro `(def ~(head args) ~(cons 'fn (tail args)))))

(def defmacro (builtin.macro `(def ~(head args) ~(cons 'macro (tail args)))))

(defmacro switch argz `(provide ~(head argz) ~(cons 'switch-blocks (tail argz))))

(defn third args (head (tail (tail (head args)))))

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

(defmacro handle-error (name) `(switch ~name ((:ok tmp) (def ~name tmp)) ((:err msg) (panic msg))))

(def libc (load-shared-library "libc.so.6"))
(handle-error libc)

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

(def fprintf
     (extern-pointer libc "fprintf"
		     ((pointer string) sizet)))
(handle-error fprintf)

(def fprintf-bytestring
     (extern-pointer libc "fprintf"
		     ((pointer string string) sizet)))
(handle-error fprintf-bytestring)

(def stdin
     (extern-pointer libc "stdin" pointer))
(handle-error stdin)

(def stdout
     (extern-pointer libc "stdout" pointer))
(handle-error stdout)

(def stderr
     (extern-pointer libc "stderr" pointer))
(handle-error stderr)
