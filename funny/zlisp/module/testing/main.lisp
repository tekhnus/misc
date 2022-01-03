!(require "stdmacro")
(require "std")

!(#defun fntest (body expect)
   (return `(progn
              !(#defun calltest () ~body)
              (def val (calltest))
              (if (eq val ~expect)
                  '()
                (def panics (cons (concat-bytestrings (concat-bytestrings (repr val) " != ") (repr ~expect)) panics))))))
