(require "zlisp")

!(#def-or-panica prelude_ (prelude))

(def panics '())




!(#test
 "hello, world!"
 "hello, world!")

!(#test
  (+ 4 3)
  7)




!(#defun print-all (xs)
   (return
   (if xs
       (progn
         (print (head xs))
         (print-all (tail xs)))
     '())))

(if panics
    (progn
      (print-all panics)
      (panic "FAILED"))
  (progn))
