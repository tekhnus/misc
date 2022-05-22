(req (std-pre-defun "std-pre-defun"))
(importall std-pre-defun)

!(req (defunmod "defun"))
!(importall defunmod)


!(#defun append (x xs)
  (if xs
      (return (cons
       (head xs)
       (append
	x
	(tail xs))))
    (return (list x))))
