(req
 (std "std")
 (eq "std" eq)
 (decons-pat "std" decons-pat)
 (head "std" head)
 (concat-bytestrings "std" concat-bytestrings)
 (panic "std" panic))

(defn fntest (body expect)
  (return (list 'progn
             (list 'defn 'calltest '() body)
             '(def val (calltest))
             (list 'if (list '/std/eq 'val expect)
                 '(def panics panics)
               (list 'def 'panics (list '/std/cons (list '/std/concat-bytestrings '(/std/concat-bytestrings (/std/repr val) " != ") (list '/std/repr expect)) 'panics))))))

(export (fntest fntest))
