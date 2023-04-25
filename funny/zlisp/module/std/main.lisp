(req
 (prelude "prelude")
 (panic- "prelude" panic)
 (head- "prelude" head)
 (tail- "prelude" tail)
 (cons- "prelude" cons)
 (eq- "prelude" eq)
 (annotate- "prelude" annotate)
 (is-constant- "prelude" is-constant)
 (repr- "prelude" repr)
 (concat-bytestrings- "prelude" concat-bytestrings)
 (+- "prelude" +))

(defn panic (x) (return (/prelude/panic- x)))
(defn head (x) (return (/prelude/head- x)))
(defn tail (x) (return (/prelude/tail- x)))
(defn cons (x xs) (return (/prelude/cons- x xs)))
(defn eq (x y) (return (/prelude/eq- x y)))
(defn annotate (x) (return (/prelude/annotate- x)))
(defn is-constant (x) (return (/prelude/is-constant- x)))
(defn repr (x) (return (/prelude/repr- x)))
(defn concat-bytestrings (x y) (return (/prelude/concat-bytestrings- x y)))
(defn + (x y) (return (/prelude/+- x y)))

(defn last (a0)
  
  (if (../tail a0)
      (return (../last (../tail a0)))
    (return (../head a0))))

(defn type (x) (return (../head (../annotate x))))

(defn length (x)
  (progn
    (def n 0)
    (while x (progn
      (def n (../+ n 1))
      (def x (../tail x))))
    (return n)))

(defn concat (a0 a1)
  
  (if a0
      (return (../cons (../head a0) (../concat (../tail a0) a1)))
    (return a1)))



(defn zip (a0 a1)
  
  (if a0
      (return (../cons `(~(../head a0) ~(../head a1)) (../zip (../tail a0) (../tail a1))))
    (return '())))

(defn map	     (a0 a1)
  (if a1
      (return (../cons
               (../a0
	        (../head a1))
               (../map
	        a0
	        (../tail a1))))
    (return '())))

(defn ignore (x) (return `(def throwaway ~x)))

(def panic-block '(argz (/std/panic "wrong fn call")))

(defn list-at (xs n) (progn
                       (if (../eq n 0)
                           (return (../head xs))
                         (return (../list-at (../tail xs) (../+ n -1))))))

(defn swtchone (a0)
  (if a0
      (progn
	(def firstarg (../head a0))
	(def cond (../head firstarg))
	(def body (../list-at firstarg 1))
	(def rest (../swtchone (../tail a0)))
	(return `(progn
		   (def prearg ~cond)
		   (if (/std/eq (/std/head prearg) :ok)
		       (progn
			 (def args (/std/list-at prearg 1))
			 ~body)
		     ~rest))))
    (progn
      (def firstarg "ifhack")
      (def cond "ifhack")
      (def body "ifhack")
      (def rest "ifhack")
      (return '(/std/panic "nothing matched")))))


(defn decons-pat (a0 a1)
  (progn
    (def pat a0)
    (def val a1)
    (if (../is-constant pat)
        (progn
          (def first-decons "ifhack")
          (def rest-decons "ifhack")
	  (if (../eq pat val)
	      (return '(:ok ()))
	    (return '(:err))))
      (if (../eq (../type pat) :symbol)
	  (progn
            (def first-decons "ifhack")
            (def rest-decons "ifhack")
            (return `(:ok (~val))))
	(if (../eq (../type pat) :list)
	    (if pat
		(if val
		    (progn
		      (def first-decons (../decons-pat (../head pat) (../head val)))
		      (def rest-decons (../decons-pat (../tail pat) (../tail val)))
		      (if (../eq :err (../head rest-decons))
			  (return '(:err))
			(if (../eq :err (../head first-decons))
			    (return '(:err))
			  (return `(:ok ~(../concat (../list-at first-decons 1) (../list-at rest-decons 1)))))))
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
            (../panic "decons-pat met an unsupported type")))))))

(defn decons-vars (a0)
  (if (../is-constant a0)
      (return '())
    (if (../eq (../type a0) :symbol)
	(return `(~a0))
      (if (../eq (../type a0) :list)
	  (if a0
	      (return (../concat (../decons-vars (../head a0)) (../decons-vars (../tail a0))))
	    (return `()))
	(panic "decons-var met an unsupported type")))))

(def switch-defines '((/std/list-at args 0) (/std/list-at args 1) (/std/list-at args 2) (/std/list-at args 3) (/std/list-at args 4) (/std/list-at args 5)))

(defn make-def (x) (return (../cons 'def x)))

(defn switch-clause (a0)
  (progn
    (def sig (../head a0))
    (def cmds (../tail a0))
    (def checker `(/std/decons-pat (quote ~sig) args))
    (def vars (../decons-vars sig))
    (def body (../cons 'progn (../concat (../map make-def (../zip vars switch-defines)) cmds)))
    (return `(~checker ~body))))

(defn switch-fun (a0)
  (return (../swtchone (../map switch-clause a0))))

(defn append (x xs)
  (if xs
      (return (../cons
               (../head xs)
               (../append
	        x
	        (../tail xs))))
    (return `(~x))))

(defn first-good-value (x) (progn
                             (if x
                                 (progn
                                   (def first-arg (../head x))
                                   (if (../eq :ok (../head first-arg))
                                       (progn
                                         (return (../list-at first-arg 1)))
                                     (return (../first-good-value (../tail x)))))
                               (panic "first-good-value: no good value"))))

(defn not (x) (if x (return '()) (return '(()))))

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
 (length length)
 (decons-pat decons-pat)
 (append append)
 (ignore ignore)
 (list-at list-at)
 (switch-fun switch-fun)
 (first-good-value first-good-value)
 (type type)
 (not not))
