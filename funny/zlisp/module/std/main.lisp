(req (std-pre-defun "std-pre-defun"))
(importall std-pre-defun)

!(require "defun")

!(#defun append (x xs)
  (if xs
      (return (cons
       (head xs)
       (append
	x
	(tail xs))))
    (return (list x))))
