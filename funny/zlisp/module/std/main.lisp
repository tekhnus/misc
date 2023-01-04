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

(builtin.defn last (a0)
	      
	 (if (tail a0)
	     (return (last (tail a0)))
	   (return (head a0))))

(def type (builtin.fn (x) (return (head (annotate x)))))

(builtin.defn concat (a0 a1)
	     
    (if a0
	(return (cons (head a0) (concat (tail a0) a1)))
      (return a1)))



(builtin.defn zip (a0 a1)
	      
    (if a0
	(return (cons `(~(head a0) ~(head a1)) (zip (tail a0) (tail a1))))
      (return '())))

(builtin.defn map	     (a0 a1)
  (if a1
      (return (cons
       (a0
	(head a1))
       (map
	a0
	(tail a1))))
    (return '())))

(def ignore-fn (builtin.fn (x) (return `(def throwaway ~x))))

(def ignore (builtin.fn (x) (return (ignore-fn x))))

(def panic-block '(argz (panic "wrong fn call")))

(def progn- (builtin.fn (x) (return (cons 'progn x))))


(builtin.defn list-at (xs n) (progn
                               (if (eq n 0)
                                   (return (head xs))
                                 (return (list-at (tail xs) (+ n -1))))))

(builtin.defn swtchone (a0)
	      (if a0
		  (progn
		    (def firstarg (head a0))
		    (def cond (head firstarg))
		    (def body (list-at firstarg 1))
		    (def rest (swtchone (tail a0)))
		    (return `(progn
			       (def prearg ~cond)
			       (if (eq (head prearg) :ok)
				   (progn
				     (def args (list-at prearg 1))
				     ~body)
				 ~rest))))
		(progn
                  (def firstarg "ifhack")
                  (def cond "ifhack")
                  (def body "ifhack")
                  (def rest "ifhack")
                  (return '(panic "nothing matched")))))


(builtin.defn decons-pat (a0 a1)
	      (progn
		(def pat a0)
		(def val a1)
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
				      (return `(:ok ~(concat (list-at first-decons 1) (list-at rest-decons 1)))))))
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

(builtin.defn decons-vars (a0)
     (if (is-constant a0)
	(return '())
      (if (eq (type a0) :symbol)
	  (return `(~a0))
	(if (eq (type a0) :list)
	    (if a0
		(return (concat (decons-vars (head a0)) (decons-vars (tail a0))))
	      (return `()))
	  (panic "decons-var met an unsupported type")))))

(def switch-defines '((list-at args 0) (list-at args 1) (list-at args 2) (lits-at args 3) (list-at args 4) (list-at args 5)))

(builtin.defn switch-clause (a0)
    (progn
      (def sig (head a0))
      (def cmds (tail a0))
      (def checker `(decons-pat (quote ~sig) args))
      (def vars (decons-vars sig))
      (def body (cons 'progn (concat (map (builtin.fn (x) (return (cons 'def x))) (zip vars switch-defines)) cmds)))
      (return `(~checker ~body))))

(builtin.defn switch-fun (a0)
    (return (swtchone (map switch-clause a0))))
(builtin.defn append (x xs)
  (if xs
      (return (cons
       (head xs)
       (append
	x
	(tail xs))))
    (return `(~x))))

(builtin.defn first-good-value (x) (progn
                                 (if x
                                     (progn
                                       (def first-arg (head x))
                                       (if (eq :ok (head first-arg))
                                           (progn
                                             (return (list-at first-arg 1)))
                                         (return (first-good-value (tail x)))))
                                   (panic "first-good-value: no good value"))))

(export
 (panic panic)
 (head head)
 (tail tail)
 (cons cons)
 (eq eq)
 (eq eq)
 (annotate annotate)
 (is-constant is-constant)
 (repr repr)
 (concat-bytestrings concat-bytestrings)
 (+ +)
 (decons-pat decons-pat)
 (append append)
 (ignore ignore)
 (list-at list-at)
 (switch-fun switch-fun)
 (first-good-value first-good-value))
