(def readme "The most basic fuctions. This module is always loaded implicitly at the start.")


(builtin.defn last
	      
	 (if (tail (head args))
	     (return (last (tail (head args))))
	   (return (head (head args)))))

(def second
     (builtin.fn
      (return (head (tail (head args))))))

(def type (builtin.fn (return (head (annotate (head args))))))

(builtin.defn concat
	     
    (if (head args)
	(return (cons (head (head args)) (concat (tail (head args)) (second args))))
      (return (second args))))

(builtin.defn decons-pat
	      (progn
		(def pat (head args))
		(def val (second args))
		(if (is-constant pat)
		    (if (eq pat val)
			(return '(:ok ()))
		      (return '(:err)))
		  (if (eq (type pat) :symbol)
		      (return (list :ok (list val)))
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
				      (return (list :ok (concat (second first-decons) (second rest-decons)))))))
			      (return '(:err)))
			  (if val
			      (return '(:err))
			    (return '(:ok ()))))
		      (panic "decons-pat met an unsupported type"))))))


(builtin.defn zip
	      
    (if (head args)
	(return (cons (list (head (head args)) (head (second args))) (zip (tail (head args)) (tail (second args)))))
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

(def list (builtin.fn (return args)))

(def ignore-fn (builtin.fn (return `(def throwaway ~(head args)))))

(def ignore (builtin.fn (return (ignore-fn (head args)))))

(def panic-block '(argz (panic "wrong fn call")))

(def progn- (builtin.fn (return (cons 'progn (head args)))))

(builtin.defn third (return (head (tail (tail (head args))))))

