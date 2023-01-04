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
    (builtin.defn twice (arg) (return (+ arg arg)))
    (return (twice 35)))
  70)

!(#fntest
  (progn
    (builtin.defn adder (n) (return !(#fn (m) (return (+ n m)))))
    (return ((adder 3) 4)))
  7)

!(#fntest
  (progn
    (builtin.defn fib () (progn
       (return 3)
       (return 5)
       (return 8)
       (return 13)))
    (def (x) (@fib))
    (def (y) (@fib '()))
    (def (z) (@fib '()))
    (def (t) (@fib '()))
    (return `(~x ~y ~z ~t)))
  '(3 5 8 13))

!(#fntest
  (progn
    (builtin.defun fff (x) (return (+ x 42)))
    (def yyy (fff 1))
    (return yyy))
  43)

!(#fntest
  (progn
    (builtin.defn multi-ret () (return 42 34))
    (def (x y) (multi-ret @2))
    (return `(~x ~y)))
  '(42 34))

!(#fntest
  (progn
    (def y 3)
    (builtin.defun fff ()
                   (progn
                     (def x 2)
                     (builtin.defun ggg ()
                                    (progn
                                                           (return (+ x 40))))
                     (return ggg)))
    (def ggg-in-fff (@fff))
    (return (fff @slash ggg-in-fff)))
  42)

(builtin.defn print-all (xs)
   (if xs
       (progn
         (print (head xs))
         (print-all (tail xs))
         (return '()))
     (return '())))

(if panics
    (progn
      (print-all panics)
      (panic "FAILED"))
  (progn '() '()))
