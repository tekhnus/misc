(req
 (eq "std" eq)
 (decons-pat "std" decons-pat)
 (head "std" head)
 (concat-bytestrings "std" concat-bytestrings)
 (panic "std" panic))

(builtin.defun fntest (body expect)
   (return `(progn
              (builtin.defun calltest () ~body)
              (def val (calltest))
              (if (eq val ~expect)
                  (def panics panics)
                (def panics (cons (concat-bytestrings (concat-bytestrings (repr val) " != ") (repr ~expect)) panics))))))

(export (fntest fntest))
