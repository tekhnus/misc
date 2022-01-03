(require "std-pre-defun")

(def defun (builtin.fn (return `(builtin.defn ~(head args) ~(switch-fun `(~(tail args)))))))
