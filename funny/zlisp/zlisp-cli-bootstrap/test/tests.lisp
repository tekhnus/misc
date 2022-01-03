(require "zlisp")
!(require "testing")
!(require "stdmacro")


!(#def-or-panica prelude_ (prelude))

(def panics '())




!(#test
 "hello, world!"
 "hello, world!")

!(#test
  (+ 4 3)
  7)

!(#test
  (second '(1 2))
  2)

!(#test
  (eq :foo :bar)
  '())

!(#test
  (progn
    (def bar :foo)
    (eq :foo bar))
  '(()))

!(#test
  (append 5 '(1 2 3 4))
  '(1 2 3 4 5))

!(#test
  (list 1 2 (+ 1 2))
  '(1 2 3))

!(#test
  (progn
    !(#defun twice (arg) (return (+ arg arg)))
    (twice 35))
  70)

!(#test
  (progn
    !(#defun adder (n) (return !(#fn (m) (return (+ n m)))))
    ((adder 3) 4))
  7)

!(#test
  (progn
    !(#defun fib ()
       (yield 3)
       (yield 5)
       (yield 8)
       (yield 13))
    !(#def2 x fib (fib))
    !(#def2 y fib (fib))
    !(#def2 z fib (fib))
    !(#def2 t fib (fib))
    (list x y z t))
  '(3 5 8 13))

!(#test
  (progn
    !(#defun far-fib ()
       (^yield 3)
       (^yield 5)
       (return 8))

    !(#defun more-far-fib ()
       (def x (far-fib))
       (^yield x)
       (^yield 13))

    !(#def2 x more-far-fib (^more-far-fib))
    !(#def2 y more-far-fib (^more-far-fib))
    !(#def2 z more-far-fib (^more-far-fib))
    !(#def2 t more-far-fib (^more-far-fib))
    (list x y z t))
  '(3 5 8 13))



(require "libc")

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
