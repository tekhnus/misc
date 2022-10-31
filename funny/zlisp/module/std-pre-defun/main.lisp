(req
 (panic "prelude" panic)
 (head "prelude" head)
 (tail "prelude" tail)
 (cons "prelude" cons)
 (eq "prelude" eq)
 (annotate "prelude" annotate)
 (is-constant "prelude" is-constant)
 (repr "prelude" repr)
 (concat-bytestrings "prelude" concat-bytestrings)
 (+ "prelude" +))

(def second
     (builtin.fn (x)
      (return (head (tail x)))))

(builtin.defn last
	      
	 (if (tail (head args))
	     (return (last (tail (head args))))
	   (return (head (head args)))))

(def type (builtin.fn (x) (return (head (annotate x)))))

(builtin.defn concat
	     
    (if (head args)
	(return (cons (head (head args)) (concat (tail (head args)) (second args))))
      (return (second args))))



(builtin.defn zip
	      
    (if (head args)
	(return (cons `(~(head (head args)) ~(head (second args))) (zip (tail (head args)) (tail (second args)))))
      (return '())))

(builtin.defn map	    
  (if (head (tail args))
      (return (cons
       ((head args)
	(head (head (tail args))))
       (map
	(head args)
	(tail (head (tail args))))))
    (return '())))

(def ignore-fn (builtin.fn (x) (return `(def throwaway ~x))))

(def ignore (builtin.fn (x) (return (ignore-fn x))))

(def panic-block '(argz (panic "wrong fn call")))

(def progn- (builtin.fn (x) (return (cons 'progn x))))

(builtin.defn third (return (head (tail (tail (head args))))))
(builtin.defn fourth (return (head (tail (tail (tail (head args)))))))
(builtin.defn fifth (return (head (tail (tail (tail (tail (head args))))))))
(builtin.defn sixth (return (head (tail (tail (tail (tail (tail (head args)))))))))

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
		(progn
                  (def firstarg "ifhack")
                  (def cond "ifhack")
                  (def body "ifhack")
                  (def rest "ifhack")
                  (return '(panic "nothing matched")))))


(builtin.defn decons-pat
	      (progn
		(def pat (head args))
		(def val (second args))
		(if (is-constant pat)
                    (progn
                      (def first-decons "ifhack")
                      (def rest-decons "ifhack")
		      (if (eq pat val)
			  (return '(:ok ()))
		        (return '(:err))))
		  (if (eq (type pat) :symbol)
		      (progn
                        (def first-decons "ifhack")
                        (def rest-decons "ifhack")
                        (return `(:ok (~val))))
		    (if (eq (type pat) :list)
			(if pat
			    (if val
				(progn
				  (def first-decons (decons-pat (head pat) (head val)))
				  (def rest-decons (decons-pat (tail pat) (tail val)))
				  (if (eq :err (head rest-decons))
				      (return '(:err))
				    (if (eq :err (head first-decons))
					(return '(:err))
				      (return `(:ok ~(concat (second first-decons) (second rest-decons)))))))
			      (progn
                                (def first-decons "ifhack")
                                (def rest-decons "ifhack")
                                (return '(:err))))
			  (if val
                              (progn
                                (def first-decons "ifhack")
                                (def rest-decons "ifhack")
			        (return '(:err)))
			    (progn
                              (def first-decons "ifhack")
                              (def rest-decons "ifhack")
                              (return '(:ok ())))))
		      (progn
                        (def first-decons "ifhack")
                        (def rest-decons "ifhack")
                        (panic "decons-pat met an unsupported type")))))))

(builtin.defn decons-vars
     (if (is-constant (head args))
	(return '())
      (if (eq (type (head args)) :symbol)
	  (return `(~(head args)))
	(if (eq (type (head args)) :list)
	    (if (head args)
		(return (concat (decons-vars (head (head args))) (decons-vars (tail (head args)))))
	      (return `()))
	  (panic "decons-var met an unsupported type")))))

(def switch-defines '((head args) (second args) (third args) (fourth args) (fifth args) (sixth args)))

(builtin.defn switch-clause
    (progn
      (def sig (head (head args)))
      (def cmds (tail (head args)))
      (def checker `(decons-pat (quote ~sig) args))
      (def vars (decons-vars sig))
      (def body (cons 'progn (concat (map (builtin.fn (x) (return (cons 'def x))) (zip vars switch-defines)) cmds)))
      (return `(~checker ~body))))

(builtin.defn switch-fun
    (return (swtchone (map switch-clause (head args)))))

(export
 (switch-fun switch-fun)
 (decons-pat decons-pat)
 (ignore ignore)
 (second second)
 (third third)
 (fourth fourth)
 (fifth fifth)
 (sixth sixth))
