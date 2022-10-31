(req
 (head "prelude" head)
 (tail "prelude" tail)
 (switch-fun "std-pre-defun" switch-fun))

(def defun (builtin.fn (x y z) (return `(builtin.defn ~x ~y ~z))))



(export (defun defun))
