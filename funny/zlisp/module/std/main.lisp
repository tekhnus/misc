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
 (second "std-pre-defun" second)
 (third "std-pre-defun" third)
 (fourth "std-pre-defun" fourth)
 (fifth "std-pre-defun" fifth)
 (sixth "std-pre-defun" sixth))

!(req (defun "defun" defun))

!(#defun append (x xs)
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
                                             (return (second first-arg)))
                                         (return ((resolve first-good-value) (tail x)))))
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
 (second second)
 (append append)
 (ignore ignore)
 (third third)
 (fourth fourth)
 (fifth fifth)
 (sixth sixth)
 (switch-fun switch-fun)
 (first-good-value first-good-value))
