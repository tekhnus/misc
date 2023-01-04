(req
 (std "std")
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (tail "std" tail)
 (repr "std" repr)
 (append "std" append)
 (list-at "std" list-at)
 (+ "std" +)
 (panic "std" panic)
 (cons "std" cons)
 (print "prelude" print)
 (tail "std" tail)
 (panic "std" panic)
 (concat-bytestrings "std" concat-bytestrings))

!(req
  (testing "testing")
  (fntest "testing" fntest))

(def panics '())

!(#testing @slash fntest
  (return (head '(42 5 3)))
  42)

!(#testing @slash fntest
  (return (tail '(42 5 3)))
  '(5 3))

!(#testing @slash fntest
  (return (head (tail '(42 5 3))))
  5)

!(#testing @slash fntest
  (return (std @slash list-at '(42 5 3) 1))
  5)

!(#testing @slash fntest
 (return "hello, world!")
 "hello, world!")

!(#testing @slash fntest
  (return (+ 4 3))
  7)

!(#testing @slash fntest
  (return (std @slash list-at '(1 2) 1))
  2)

!(#testing @slash fntest
  (return (eq :foo :bar))
  '())

!(#testing @slash fntest
  (progn
    (def bar :foo)
    (return (eq :foo bar)))
  '(()))

!(#testing @slash fntest
  (progn
    (return (std @slash append 5 '(1 2 3 4))))
  '(1 2 3 4 5))

!(#testing @slash fntest
  (return `(1 2 ~(+ 1 2)))
  '(1 2 3))

!(#testing @slash fntest
  (progn
    (builtin.defun twice (arg) (return (+ arg arg)))
    (return (twice 35)))
  70)

!(#testing @slash fntest
  (progn
    (builtin.defun adder (n) (return (builtin.fn (m) (return (+ n m)))))
    (return ((adder 3) 4)))
  7)

!(#testing @slash fntest
  (progn
    (builtin.defun fib () (progn
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

!(#testing @slash fntest
  (progn
    (builtin.defun fff (x) (return (+ x 42)))
    (def yyy (fff 1))
    (return yyy))
  43)

!(#testing @slash fntest
  (progn
    (builtin.defun multi-ret () (return 42 34))
    (def (x y) (multi-ret @2))
    (return `(~x ~y)))
  '(42 34))

!(#testing @slash fntest
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

(builtin.defun print-all (xs)
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
