(req
 (switch-fun "std" switch-fun)
 (ignore "std" ignore)
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (second "std" second)
 (third "std" third)
 (panic "std" panic)
 (tail "std" tail))

(def fn (builtin.fn (x y) (return `(builtin.fn ~x ~y))))

(builtin.defn def2 (left right val)
   (return `(progn
	      (def tmp ~val)
	      (def ~left (head tmp))
	      (def ~right (second tmp)))))

(builtin.defn switchx2 (exp argz) (return `(progn (def args ~exp) ~(switch-fun argz))))

(export
 (fn fn)
 (def2 def2)
 (switchx2 switchx2))
