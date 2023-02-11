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
             (if (/std/eq val ~expect)
                 (def panics panics)
               (def panics (/std/cons (/std/concat-bytestrings (/std/concat-bytestrings (/std/repr val) " != ") (/std/repr ~expect)) panics))))))

(export (fntest fntest))
