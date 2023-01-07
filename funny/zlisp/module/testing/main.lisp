(req
 (std "std")
 (eq "std" eq)
 (decons-pat "std" decons-pat)
 (head "std" head)
 (concat-bytestrings "std" concat-bytestrings)
 (panic "std" panic))

(defn fntest (body expect)
   (return `(progn
              (defn calltest () ~body)
              (def val (calltest))
              (if (std @slash eq val ~expect)
                  (def panics panics)
                (def panics (std @slash cons (std @slash concat-bytestrings (std @slash concat-bytestrings (std @slash repr val) " != ") (std @slash repr ~expect)) panics))))))

(export (fntest fntest))
