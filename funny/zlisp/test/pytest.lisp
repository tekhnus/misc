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
 (prelude "prelude")
 (print "prelude" print)
 (concat-bytestrings "std" concat-bytestrings))

(def panics '())

(fntest
  (return (std/head '(42 5 3)))
  42)

(fntest
  (return (std/tail '(42 5 3)))
  '(5 3))

(fntest
  (return (std/head (std/tail '(42 5 3))))
  5)

(fntest
  (return (std/list-at '(42 5 3) 1))
  5)

(fntest
 (return "hello, world!")
 "hello, world!")

(fntest
  (return (std/+ 4 3))
  7)

(fntest
  (return (list (/std/+ 4 3) 8))
  '(7 8))

(fntest
  (return (std/list-at '(1 2) 1))
  2)

(fntest
  (return (std/eq :foo :bar))
  '())

(fntest
  (progn
    (def bar :foo)
    (return (std/eq :foo bar)))
  '(()))

(fntest
  (progn
    (return (std/append 5 '(1 2 3 4))))
  '(1 2 3 4 5))

(fntest
  (return `(1 2 ~(std/+ 1 2)))
  '(1 2 3))

(fntest
  (progn
    (defn twice (arg) (return (std/+ arg arg)))
    (return (twice 35)))
  70)

(fntest
  (progn
    (defn adderf (n)
                   (progn
                     (def m (return @1))
                     (return (std/+ n m))))
    (defn adder (n)
                   (progn
                     (def a adderf)
                     (def () (a @0 @mut n))
                     (return a)))
    (return ((adder 3) 4)))
  7)

(fntest
  (progn
    (defn fib () (progn
       (return @0 3)
       (return @0 5)
       (return @0 8)
       (return @0 13)))
    (def (x) (fib @mut))
    (def (y) (fib @mut))
    (def (z) (fib @mut))
    (def (t) (fib @mut))
    (return `(~x ~y ~z ~t)))
  '(3 5 8 13))

(fntest
  (progn
    (defn fff (x) (return (std/+ x 42)))
    (def yyy (fff 1))
    (return yyy))
  43)

(fntest
  (progn
    (defn multi-ret () (return 42 34))
    (def (x y) (multi-ret @2))
    (return `(~x ~y)))
  '(42 34))

(fntest
  (progn
    (def y 3)
    (defn fff ()
                   (progn
                     (def x 2)
                     (defn ggg ()
                                    (progn
                                                           (return (std/+ x 40))))
                     (return ggg)))
    (def ggg-in-fff (fff @mut))
    (return (fff/ggg-in-fff)))
  42)

(defn print-all (xs)
   (if xs
       (progn
         (def res (/prelude/print (/std/head xs)))
         (def () (../print-all @0 (/std/tail xs)))
         (return))
     (return)))

(if panics
    (progn
      (def () (print-all @0 panics))
      (def () (/std/panic @0 "FAILED"))
      (def x 42))
  (def x 33))
