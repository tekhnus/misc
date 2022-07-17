(req
 (eq "std" eq)
 (decons-pat "std" decons-pat)
 (head "std" head)
 (concat-bytestrings "std" concat-bytestrings)
 (panic "std" panic)
 (second "std" second))

!(req
  (defun "stdmacro" defun)
  (fn "stdmacro" fn)
  (def2 "stdmacro" def2))


!(#defun fntest (body expect)
   (return `(progn
              !(#defun calltest () ~body)
              (def val (calltest))
              (def panics ((builtin.fn
                            (if (eq val ~expect)
                                (return panics)
                              (return (cons (concat-bytestrings (concat-bytestrings (repr val) " != ") (repr ~expect)) panics)))))))))

(export (fntest fntest))
