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
 (+ "prelude" +)
 (decons-pat "std-pre-defun" decons-pat)
 (switch-fun "std-pre-defun" switch-fun)
 (ignore "std-pre-defun" ignore)
 (list-at "std-pre-defun" list-at))

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
