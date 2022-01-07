!(require "defun")
(require "defun")
(require "std")

(def fn (builtin.fn (return `(builtin.fn ~(switch-fun `(~args))))))

!(#defun def2 (left right val)
   (return `(progn
	      (def tmp ~val)
	      (def ~left (head tmp))
	      (def ~right (second tmp)))))


!(#defun def-or-panic-tmp-fn (arg)
  (if arg
      (return `(progn
	 (def tmp ~(head arg))
	 (if (eq :err (head tmp))
	     ~(def-or-panic-tmp-fn (tail arg))
	     (progn))))
    (return `(panic (second tmp)))))

(def def-or-panica
     (builtin.fn
      (return
      `(progn
	 ~(def-or-panic-tmp-fn (tail args))
	 (def ~(head args) (second tmp))))))

!(#defun wrap-fn-pointer (fnname ptrname) (return `(def ~fnname (builtin.fn (return (pointer-call ~ptrname args))))))

!(#defun switchx argz (return `(progn (def args ~(head argz)) ~(switch-fun (tail argz)))))
