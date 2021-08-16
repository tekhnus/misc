(def readme "The most basic fuctions. This module is always loaded implicitly at the start.")



(builtin.defn last
	 (if (tail (head args))
	     (last (tail (head args)))
	   (head (head args))))



(def quote (builtin.operator  (head args)))

(def second
     (builtin.fn
      (head (tail (head args)))))

(def type (builtin.fn (head (annotate (head args)))))

(builtin.defn concat
    (if (head args)
	(cons (head (head args)) (concat (tail (head args)) (second args)))
      (second args)))

(builtin.defn decons-pat
	      (progn
		(def pat (head args))
		(def val (second args))
		(if (is-constant pat)
		    (if (eq pat val)
			`(:ok ())
		      `(:err))
		  (if (eq (type pat) :symbol)
		      `(:ok (~val))
		    (if (eq (type pat) :list)
			(if pat
			    (if val
				(progn
				  (def first-decons (decons-pat (head pat) (head val)))
				  (def rest-decons (decons-pat (tail pat) (tail val)))
				  (if (eq :err (head rest-decons))
				      `(:err)
				    (if (eq :err (head first-decons))
					`(:err)
				      `(:ok ~(concat (second first-decons) (second rest-decons))))))
			      `(:err))
			  (if val
			      `(:err)
			    `(:ok ())))
		      (panic "decons-pat met an unsupported type"))))))

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

(def list (builtin.fn args))

(def ignore-fn (builtin.fn `(def throwaway ~(head args))))

(def ignore (builtin.operator (ignore-fn (head args))))

(def panic-block '(argz (panic "wrong fn call")))

(def progn- (builtin.fn (cons 'progn (head args))))

(def defun (builtin.operator `(builtin.defn ~(head args) ~(switch-fun `(~(tail args))))))

(def defop (builtin.operator `(def ~(head args) (builtin.operator ~(switch-fun `(~(tail args)))))))

!(defop switchx argz `(progn (def args ~(head argz)) ~(switch-fun (tail argz))))

!(defun third args (head (tail (tail (head args)))))

!(defun append (x xs)
  (if xs
      (cons
       (head xs)
       (append
	x
	(tail xs)))
    (list x)))


!(defun def-or-panic-tmp-fn (arg)
  (if arg
      `(progn
	 (def tmp ~(head arg))
	 (if (eq :err (head tmp))
	     ~(def-or-panic-tmp-fn (tail arg))
	     (progn)))
    `(panic (second tmp))))

(def def-or-panica
     (builtin.operator
      `(progn
	 ~(def-or-panic-tmp-fn (tail args))
	 (def ~(head args) (second tmp)))))

!(def-or-panica libc
  (shared-library "libc.so.6")
  (shared-library "libSystem.B.dylib"))

!(def-or-panica malloc
     (extern-pointer libc "malloc"
		     '((sizet) pointer)))

!(def-or-panica fopen
     (extern-pointer libc "fopen"
		     '((string string) pointer)))

!(def-or-panica fread
     (extern-pointer libc "fread"
		     '((pointer sizet sizet pointer) sizet)))

!(def-or-panica feof
     (extern-pointer libc "feof"
		     '((pointer) int)))

!(def-or-panica fprintf
     (extern-pointer libc "fprintf"
		     '((pointer string) sizet)))

!(def-or-panica fprintf-bytestring
     (extern-pointer libc "fprintf"
		     '((pointer string string) sizet)))

!(def-or-panica stdin
  (extern-pointer libc "stdin" 'pointer)
  (extern-pointer libc "__stdinp" 'pointer))

!(def-or-panica stdout
  (extern-pointer libc "stdout" 'pointer)
  (extern-pointer libc "__stdoutp" 'pointer))

!(def-or-panica stderr
  (extern-pointer libc "stderr" 'pointer)
  (extern-pointer libc "__stderrp" 'pointer))

!(defop print (val)
  (ignore-fn `(fprintf-bytestring stdout "%s\n" ~(repr val))))
