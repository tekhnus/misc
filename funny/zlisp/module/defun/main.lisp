(req
 (head "prelude" head)
 (tail "prelude" tail)
 (switch-fun "std-pre-defun" switch-fun))

(def defun (builtin.fn (return `(builtin.defn ~(head args) ~(switch-fun `(~(tail args)))))))

(export (defun defun))
