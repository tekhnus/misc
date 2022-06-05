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
 (list "std-pre-defun" list))

!(req (defun "defun" defun))

!(#defun append (x xs)
  (if xs
      (return (cons
       (head xs)
       (append
	x
	(tail xs))))
    (return (list x))))

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
 (list list)
 (ignore ignore)
 (third third)
 (switch-fun switch-fun))
