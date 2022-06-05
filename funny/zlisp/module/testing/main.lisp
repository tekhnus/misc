(req
 (eq "std" eq)
 (decons-pat "std" decons-pat)
 (head "std" head)
 (second "std" second))

!(req (stdmacro "stdmacro"))
!(importall stdmacro)


!(#defun fntest (body expect)
   (return `(progn
              !(#defun calltest () ~body)
              (def val (calltest))
              (if (eq val ~expect)
                  '()
                (def panics (cons (concat-bytestrings (concat-bytestrings (repr val) " != ") (repr ~expect)) panics))))))

(export (fntest fntest))
