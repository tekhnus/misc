(req
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (tail "std" tail)
 (repr "std" repr)
 (second "std" second)
 (append "std" append)
 (+ "std" +)
 (panic "std" panic)
 (cons "std" cons)
 (print "prelude" print)
 (tail "std" tail)
 (panic "std" panic)
 (concat-bytestrings "std" concat-bytestrings))

!(req (fntest "testing" fntest))
!(req
  (defun "stdmacro" defun)
  (fn "stdmacro" fn)
  (def2 "stdmacro" def2))

(def panics '())

!(#fntest
  (return (head '(42 5 3)))
  42)

!(#fntest
  (return (tail '(42 5 3)))
  '(5 3))

!(#fntest
  (return (head (tail '(42 5 3))))
  5)

!(#fntest
  (return (second '(42 5 3)))
  5)

!(#fntest
 (return "hello, world!")
 "hello, world!")

!(#fntest
  (return (+ 4 3))
  7)

!(#fntest
  (return (second '(1 2)))
  2)

!(#fntest
  (return (eq :foo :bar))
  '())

!(#fntest
  (progn
    (def bar :foo)
    (return (eq :foo bar)))
  '(()))

!(#fntest
  (progn
    (return (append 5 '(1 2 3 4))))
  '(1 2 3 4 5))

!(#fntest
  (return `(1 2 ~(+ 1 2)))
  '(1 2 3))

!(#fntest
  (progn
    !(#defun twice (arg) (return (+ arg arg)))
    (return (twice 35)))
  70)

!(#fntest
  (progn
    !(#defun adder (n) (return !(#fn (m) (return (+ n m)))))
    (return ((adder 3) 4)))
  7)

!(#fntest
  (progn
    !(#defun fib () (progn
       (return 3)
       (return 5)
       (return 8)
       (return 13)))
    (def (x fib) (@fib))
    (def (y fib) (@fib '()))
    (def (z fib) (@fib '()))
    (def (t fib) (@fib '()))
    (return `(~x ~y ~z ~t)))
  '(3 5 8 13))

!(#fntest
  (progn
    (builtin.defun fff (x) (return (+ x 42)))
    (def yyy ((resolve fff) 1))
    (return yyy))
  43)

!(#defun print-all (xs)
   (if xs
       (progn
         (print (head xs))
         (print-all (tail xs))
         (return '()))
     (return '())))

!(#fntest
  (progn
    (builtin.defn multi-ret () (return 42 34))
    (def (x y) (multi-ret @2))
    (return `(~x ~y)))
  '(42 34))

(if panics
    (progn
      (print-all panics)
      (panic "FAILED"))
  (progn))
