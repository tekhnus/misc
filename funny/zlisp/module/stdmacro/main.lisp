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

(builtin.defn switchx2 (exp argz) (return `(progn (def args ~exp) ~(switch-fun argz))))

(export
 (fn fn)
 (switchx2 switchx2))
