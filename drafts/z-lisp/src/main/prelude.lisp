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

(debug 777)
(debug (decons-pat 'a 7))
(debug (decons-vars 'a))
(debug (decons-pat :a :a))
(debug (decons-vars :a))
(debug (decons-pat :a :b))
(debug (decons-vars :a))
(debug (decons-pat '() '()))
(debug (decons-vars '()))
(debug (decons-pat '(a) '(7)))
(debug (decons-vars '(a)))
(debug (decons-pat '(a b) '(7 8)))
(debug (decons-vars '(a b)))
(debug (decons-pat '(:a b) '(:a 8)))
(debug (decons-vars '(:a b)))
(debug (decons-pat '(:a b) '(:b 8)))
(debug (decons-vars '(:a b)))

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

(def xxx '((head args) (second args) (third args)))
(builtin.defn switch-clause
    (progn
      (def sig (head (head args)))
      (def cmds (tail (head args)))
      (def checker `(decons-pat '~sig args))
      (def vars (decons-vars sig))
      (def body (cons 'progn (concat (map (builtin.fn (cons 'def (head args))) (zip vars xxx)) cmds)))
      `(~checker ~body)))

(builtin.defn switch-fun
    (cons 'builtin.switch (map switch-clause (head args))))

(def switch-args (builtin.macro (switch-fun args)))

(debug 999)
(debug (switch-clause '((:a b c) (print b) c)))
(debug (switch-fun '(((:a b c) (print b) c) ((x (print x))))))
(debug 444)
(def args '(:a 3 4))

(def args '(:a 3 4))
(debug (decons-pat '(:a b c) args))
(switch-args ((:a b c) (debug b) (debug c)) ((x (print x))))

(def list (builtin.fn args))

(def ignore (builtin.macro `(def throwaway ~(head args))))
(debug args)
(progn (debug args) (def foo 73) (debug foo))
(debug args)

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

(def handle-error
     (builtin.macro
      `(if (eq :err (head ~(head args)))
	   (panic (second ~(head args)))
	 (def ~(head args) (second ~(head args))))))

(def libc (shared-library "libc.so.6"))
(handle-error libc)

(def malloc
     (extern-pointer libc "malloc"
		     '((sizet) pointer)))
(handle-error malloc)

(def fread
     (extern-pointer libc "fread"
		     '((pointer sizet sizet pointer) sizet)))
(handle-error fread)

(def feof
     (extern-pointer libc "feof"
		     '((pointer) int)))
(handle-error feof)

(def fprintf
     (extern-pointer libc "fprintf"
		     '((pointer string) sizet)))
(handle-error fprintf)

(def fprintf-bytestring
     (extern-pointer libc "fprintf"
		     '((pointer string string) sizet)))
(handle-error fprintf-bytestring)

(def stdin
     (extern-pointer libc "stdin" 'pointer))
(handle-error stdin)

(def stdout
     (extern-pointer libc "stdout" 'pointer))
(handle-error stdout)

(def stderr
     (extern-pointer libc "stderr" 'pointer))
(handle-error stderr)

(def interpreter (shared-library "target/zlisp-zlisp.so"))
(handle-error interpreter)

(def read
     (extern-pointer interpreter "builtin_read" '((datum) eval_result)))
(handle-error read)

(def eval
     (extern-pointer interpreter "builtin_eval" '((datum datum) eval_result)))
(handle-error eval)

(def builtins
     (extern-pointer interpreter "builtin_builtins" '(() eval_result)))
(handle-error builtins)
