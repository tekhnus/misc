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
 (concat-bytestrings "std" concat-bytestrings))

!(req
  (testing "testing")
  (fntest "testing" fntest))

(def panics '())

!(#testing/fntest
  (return (std/head '(42 5 3)))
  42)

!(#testing/fntest
  (return (std/tail '(42 5 3)))
  '(5 3))

!(#testing/fntest
  (return (std/head (std/tail '(42 5 3))))
  5)

!(#testing/fntest
  (return (std/list-at '(42 5 3) 1))
  5)

!(#testing/fntest
 (return "hello, world!")
 "hello, world!")

!(#testing/fntest
  (return (std/+ 4 3))
  7)

!(#testing/fntest
  (return (std/list-at '(1 2) 1))
  2)

!(#testing/fntest
  (return (std/eq :foo :bar))
  '())

!(#testing/fntest
  (progn
    (def bar :foo)
    (return (std/eq :foo bar)))
  '(()))

!(#testing/fntest
  (progn
    (return (std/append 5 '(1 2 3 4))))
  '(1 2 3 4 5))

!(#testing/fntest
  (return `(1 2 ~(std/+ 1 2)))
  '(1 2 3))

!(#testing/fntest
  (progn
    (defn twice (arg) (return (std/+ arg arg)))
    (return (twice 35)))
  70)

!(#testing/fntest
  (progn
    (defn adderf (n)
                   (progn
                     (def m (return :ready))
                     (return (std/+ n m))))
    (defn adder (n)
                   (progn
                     (def a adderf)
                     (@a @mut n)
                     (return a)))
    (return ((adder 3) 4)))
  7)

!(#testing/fntest
  (progn
    (defn fib () (progn
       (return 3)
       (return 5)
       (return 8)
       (return 13)))
    (def (x) (@fib @mut))
    (def (y) (@fib @mut '()))
    (def (z) (@fib @mut '()))
    (def (t) (@fib @mut '()))
    (return `(~x ~y ~z ~t)))
  '(3 5 8 13))

!(#testing/fntest
  (progn
    (defn fff (x) (return (std/+ x 42)))
    (def yyy (fff 1))
    (return yyy))
  43)

!(#testing/fntest
  (progn
    (defn multi-ret () (return 42 34))
    (def (x y) (multi-ret @2))
    (return `(~x ~y)))
  '(42 34))

!(#testing/fntest
  (progn
    (def y 3)
    (defn fff ()
                   (progn
                     (def x 2)
                     (defn ggg ()
                                    (progn
                                                           (return (std/+ x 40))))
                     (return ggg)))
    (def ggg-in-fff (@fff @mut))
    (return (fff/ggg-in-fff)))
  42)

(defn print-all (xs)
   (if xs
       (progn
         (print (std/head xs))
         (print-all (std/tail xs))
         (return '()))
     (return '())))

(if panics
    (progn
      (print-all panics)
      (std/panic "FAILED"))
  (progn '() '()))
