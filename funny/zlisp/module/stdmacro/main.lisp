(req
 (switch-fun "std" switch-fun))

(builtin.defn switchx2 (exp argz) (return `(progn (def args ~exp) ~(switch-fun argz))))

(export
 (switchx2 switchx2))
