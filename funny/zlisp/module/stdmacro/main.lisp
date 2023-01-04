(req
 (switch-fun "std" switch-fun))

(builtin.defun switch (exp argz) (return `(progn (def args ~exp) ~(switch-fun argz))))

(export
 (switch switch))
