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

(builtin.defn switchx2 (exp argz) (return `(progn (def args ~exp) ~(switch-fun argz))))

(export
 (switchx2 switchx2))
