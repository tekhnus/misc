(builtin.defn swtchone
	      (if (head args)
		  (progn
		    (def firstarg (head (head args)))
		    (def cond (head firstarg))
		    (def body (second firstarg))
		    (def rest (swtchone (tail (head args))))
		    (return `(progn
			       (def prearg ~cond)
			       (if (eq (head prearg) :ok)
				   (progn
				     (def args (second prearg))
				     ~body)
				 ~rest))))
		(return '(panic "nothing matched"))))

(builtin.defn decons-vars
     (if (is-constant (head args))
	(return '())
      (if (eq (type (head args)) :symbol)
	  (return (list (head args)))
	(if (eq (type (head args)) :list)
	    (if (head args)
		(return (concat (decons-vars (head (head args))) (decons-vars (tail (head args)))))
	      (return `()))
	  (panic "decons-var met an unsupported type")))))

(builtin.defn switch-clause
    (progn
      (def sig (head (head args)))
      (def cmds (tail (head args)))
      (def checker (list 'decons-pat (list 'quote sig) 'args))
      (def vars (decons-vars sig))
      (def body (cons 'progn (concat (map (builtin.fn (return (cons 'def (head args)))) (zip vars switch-defines)) cmds)))
      (return (list checker body))))

(def switch-defines '((head args) (second args) (third args)))

(builtin.defn switch-fun
    (return (swtchone (map switch-clause (head args)))))
