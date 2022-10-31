(req
 (defun "defun" defun)
 (switch-fun "std" switch-fun)
 (ignore "std" ignore)
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (second "std" second)
 (third "std" third)
 (panic "std" panic)
 (tail "std" tail))

!(req (defun "defun" defun))

(def fn (builtin.fn (x y) (return `(builtin.fn ~x ~y))))

!(#defun def2 (left right val)
   (return `(progn
	      (def tmp ~val)
	      (def ~left (head tmp))
	      (def ~right (second tmp)))))

!(#defun switchx2 (exp argz) (return `(progn (def args ~exp) ~(switch-fun argz))))

(export
 (defun defun)
 (fn fn)
 (def2 def2)
 (switchx2 switchx2))
