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
     (builtin.fn (xs)
      (return (head (tail xs)))))

(builtin.defn last (xs)
	      
	 (if (tail xs)
	     (return (last (tail xs)))
	   (return (head xs))))

(def type (builtin.fn (x) (return (head (annotate x)))))

(builtin.defn concat (x y)
	     
    (if x
	(return (cons (head x) (concat (tail x) y)))
      (return y)))



(builtin.defn zip (x y)
	      
    (if x
	(return (cons `(~(head x) ~(head y)) (zip (tail x) (tail y))))
      (return '())))

(builtin.defn map	    (f xs)
  (if xs
      (return (cons
       (f
	(head xs))
       (map
	f
	(tail xs))))
    (return '())))

(def ignore-fn (builtin.fn (x) (return `(def throwaway ~x))))

(def ignore (builtin.fn (x) (return (ignore-fn x))))

(def panic-block '(argz (panic "wrong fn call")))

(def progn- (builtin.fn (x) (return (cons 'progn x))))

(builtin.defn third (xs) (return (head (tail (tail xs)))))
(builtin.defn fourth (xs) (return (head (tail (tail (tail xs))))))
(builtin.defn fifth (xs) (return (head (tail (tail (tail (tail xs)))))))
(builtin.defn sixth (xs) (return (head (tail (tail (tail (tail (tail xs))))))))

(builtin.defn swtchone (x)
	      (if x
		  (progn
		    (def firstarg x))
		    (def cond (head firstarg))
		    (def body (second firstarg))
		    (def rest (swtchone (tail x)))
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


(builtin.defn decons-pat (pat val)
	      (progn
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

(builtin.defn decons-vars (x)
     (if (is-constant x)
	(return '())
      (if (eq (type x) :symbol)
	  (return `(~x))
	(if (eq (type x) :list)
	    (if x
		(return (concat (decons-vars (head x)) (decons-vars (tail x))))
	      (return `()))
	  (panic "decons-var met an unsupported type")))))

(def switch-defines '((head args) (second args) (third args) (fourth args) (fifth args) (sixth args)))

(builtin.defn switch-clause
    (progn
      (def sig (head (head args)))
      (def cmds (tail (head args)))
      (def checker `(decons-pat (quote ~sig) args))
      (def vars (decons-vars sig))
      (def body (cons 'progn (concat (map (builtin.fn (return (cons 'def (head args)))) (zip vars switch-defines)) cmds)))
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
