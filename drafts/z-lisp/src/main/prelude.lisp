(def readme "The most basic fuctions. This module is always loaded implicitly at the start.")

(def debug (builtin.macro `(def discard ~(head args))))

(builtin.defn last
	 (if (tail (head args))
	     (last (tail (head args)))
	   (head (head args))))



(def quote (builtin.operator  (head args)))

(def second
     (builtin.fn
      (head (tail (head args)))))

(def type (builtin.fn (head (annotate (head args)))))

(builtin.defn decons-fn
      (if (is-constant (head args))
	  `(if (eq ~(head args) ~(second args)) :ok :err)
	(if (eq (type (head args)) :symbol)
	    `(progn
	       (def ~(head args) ~(second args))
	       :ok)
	  (if (eq (type (head args)) :list)
	      (if (head args)
		  `(if (eq ~(decons-fn (head (head args)) `(head ~(second args))) :err)
		       :err
		     ~(decons-fn (tail (head args)) `(tail ~(second args))))
		`(if ~(second args) :err :ok))
	    (panic "decons met an unsupported type")))))

(builtin.defn concat
    (if (head args)
	(cons (head (head args)) (concat (tail (head args)) (second args)))
      (second args)))

(builtin.defn decons-pat
    (if (is-constant (head args))
	(if (eq (head args) (second args))
	    `(:ok ())
	  `(:err))
      (if (eq (type (head args)) :symbol)
	  `(:ok (~(second args)))
	(if (eq (type (head args)) :list)
	    (if (head args)
		(if (second args)
		    (progn
		      (def first-decons (decons-pat (head (head args)) (head (second args))))
		      (def rest-decons (decons-pat (tail (head args)) (tail (second args))))
		      (if (eq :err (head rest-decons))
			  `(:err)
			(if (eq :err (head first-decons))
			    `(:err)
			  `(:ok ~(concat (second first-decons) (second rest-decons))))))
		  `(:err))
	      (if (second args)
		  `(:err)
		`(:ok ())))
	  (panic "decons-pat met an unsupported type")))))

(builtin.defn decons-vars
    (if (is-constant (head args))
	`()
      (if (eq (type (head args)) :symbol)
	  `(~(head args))
	(if (eq (type (head args)) :list)
	    (if (head args)
		(concat (decons-vars (head (head args))) (decons-vars (tail (head args))))
	      `())
	  (panic "decons-var met an unsupported type")))))

(builtin.defn zip
    (if (head args)
	(cons `(~(head (head args)) ~(head (second args))) (zip (tail (head args)) (tail (second args))))
      `()))

(builtin.defn map
  (if (head (tail args))
      (cons
       ((head args)
	(head (head (tail args))))
       (map
	(head args)
	(tail (head (tail args)))))
    '()))

(def switch-defines '((head args) (second args) (third args)))

(builtin.defn switch-clause
    (progn
      (def sig (head (head args)))
      (def cmds (tail (head args)))
      (def checker `(decons-pat '~sig args))
      (def vars (decons-vars sig))
      (def body (cons 'progn (concat (map (builtin.fn (cons 'def (head args))) (zip vars switch-defines)) cmds)))
      `(~checker ~body)))

(builtin.defn switch-fun
    (cons 'builtin.switch (map switch-clause (head args))))

(def switch-args (builtin.macro (switch-fun args)))

(def list (builtin.fn args))

(def ignore (builtin.macro `(def throwaway ~(head args))))

(def panic-block '(argz (panic "wrong fn call")))

(def progn- (builtin.fn (cons 'progn (head args))))

(def fn (builtin.macro `(builtin.fn (switch-args ~args ~panic-block))))

(def macro (builtin.macro `(builtin.macro (switch-args ~args ~panic-block))))

(def defn (builtin.macro `(builtin.defn ~(head args) ~(switch-fun `(~(tail args))))))

(def defmacro (builtin.macro `(def ~(head args) ~(cons 'macro (tail args)))))

(defmacro switch argz `(progn (def args ~(head argz)) ~(cons 'switch-args (tail argz))))

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

(def def-or-panic
     (builtin.macro
      `(progn
	 (def tmp ~(second args))
	 (if (eq :err (head tmp))
	     (panic (second tmp))
	   (def ~(head args) (second tmp))))))

(def-or-panic libc (shared-library "libc.so.6"))

(def-or-panic malloc
     (extern-pointer libc "malloc"
		     '((sizet) pointer)))

(def-or-panic fopen
     (extern-pointer libc "fopen"
		     '((string string) pointer)))

(def-or-panic fread
     (extern-pointer libc "fread"
		     '((pointer sizet sizet pointer) sizet)))

(def-or-panic feof
     (extern-pointer libc "feof"
		     '((pointer) int)))

(def-or-panic fprintf
     (extern-pointer libc "fprintf"
		     '((pointer string) sizet)))

(def-or-panic fprintf-bytestring
     (extern-pointer libc "fprintf"
		     '((pointer string string) sizet)))

(def-or-panic stdin
     (extern-pointer libc "stdin" 'pointer))

(def-or-panic stdout
     (extern-pointer libc "stdout" 'pointer))

(def-or-panic stderr
     (extern-pointer libc "stderr" 'pointer))

(def-or-panic zlisp-zlisp (shared-library "target/zlisp-zlisp.so"))

(def-or-panic read
     (extern-pointer zlisp-zlisp "read" '((datum) eval_result)))

(def-or-panic eval
     (extern-pointer zlisp-zlisp "eval" '((datum datum) eval_result)))

(def-or-panic builtins
     (extern-pointer zlisp-zlisp "builtins" '(() eval_result)))

(defmacro print (val)
  `(ignore (fprintf-bytestring stdout "%s\n" (repr ~val))))
