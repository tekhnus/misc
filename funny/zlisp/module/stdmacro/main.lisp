(req
 (std "std")
 (switch-fun "std" switch-fun))

(builtin.defun switch (exp argz) (return `(progn (def args ~exp) ~(std @slash switch-fun argz))))

(export
 (switch switch))
