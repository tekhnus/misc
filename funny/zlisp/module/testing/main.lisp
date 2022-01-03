!(require "stdmacro")

!(#defun test (expr expect)
   (return `(progn
              (def resu (eval '~expr prelude_))
              !(#switchx resu
                         ((:ok val ctxt)
                          (if (eq val ~expect)
                              '()
                            (def panics (cons (concat-bytestrings (concat-bytestrings (repr val) " != ") (repr ~expect)) panics))))
                         ((:err msg)
                          (def panics (cons (concat-bytestrings "panic: " msg) panics)))))))
