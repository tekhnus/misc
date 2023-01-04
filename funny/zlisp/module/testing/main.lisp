(req
 (eq "std" eq)
 (decons-pat "std" decons-pat)
 (head "std" head)
 (concat-bytestrings "std" concat-bytestrings)
 (panic "std" panic)
 (second "std" second))

!(req
  (fn "stdmacro" fn))


(builtin.defn fntest (body expect)
   (return `(progn
              (builtin.defn calltest () ~body)
              (def val (calltest))
              (if (eq val ~expect)
                  (def panics panics)
                (def panics (cons (concat-bytestrings (concat-bytestrings (repr val) " != ") (repr ~expect)) panics))))))

(export (fntest fntest))
