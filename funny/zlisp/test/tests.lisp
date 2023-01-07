(req
 (prelude "prelude")
 (fprintf "libc" fprintf)
 (stderr "libc" stderr)
 (std "std")
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (repr "std" repr)
 (append "std" append)
 (list-at "std" list-at)
 (+ "std" +)
 (panic "std" panic)
 (concat-bytestrings "std" concat-bytestrings)
 (cons "std" cons)
 (tail "std" tail)
 (panic "std" panic)
 (libc "libc")
 (print "libc" print))

!(req
  (testing "testing")
  (fntest "testing" fntest))

(def panics '())

!(#testing @slash fntest
 (return "hello, world!")
 "hello, world!")

!(#testing @slash fntest
  (return (std @slash + 4 3))
  7)

!(#testing @slash fntest
  (return (std @slash list-at '(1 2) 1))
  2)

!(#testing @slash fntest
  (return (std @slash eq :foo :bar))
  '())

!(#testing @slash fntest
  (progn
    (def bar :foo)
    (return (std @slash eq :foo bar)))
  '(()))

!(#testing @slash fntest
  (progn
    (return (std @slash append 5 '(1 2 3 4))))
  '(1 2 3 4 5))

!(#testing @slash fntest
  (return `(1 2 ~(std @slash + 1 2)))
  '(1 2 3))

!(#testing @slash fntest
  (progn
    (defn twice (arg) (return (std @slash + arg arg)))
    (return (twice 35)))
  70)

!(#testing @slash fntest
  (progn
    (defn adderf (n)
                   (progn
                     (def m (return :ready))
                     (return (std @slash + n m))))
    (defn adder (n)
                   (progn
                     (def a adderf)
                     (@a n)
                     (return a)))
    (return ((adder 3) 4)))
  7)

!(#testing @slash fntest
  (progn
    (defn fib () (progn
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
    (defn far-fib () (progn
       (^return 3)
       (^return 5)
       (return 8)))

    (defn more-far-fib () (progn
       (def x (far-fib))
       (^return x)
       (^return 13)))

    (def (x) (@^more-far-fib))
    (def (y) (@^more-far-fib '()))
    (def (z) (@^more-far-fib '()))
    (def (t) (@^more-far-fib '()))
    (return `(~x ~y ~z ~t)))
  '(3 5 8 13))

!(#testing @slash fntest
  (progn
    (prelude @slash fprintf stderr "hello")
    (return 42))
  42)


!(#testing @slash fntest
  (progn
    (defn multi-ret () (return 42 34))
    (def (x y) (multi-ret @2))
    (return `(~x ~y)))
  '(42 34))

!(#testing @slash fntest
           (progn
             (defn foo (x) (progn
                            (def y (return (std @slash + x 1)))
                            (def (z t) (return @2 (std @slash + y 1)))
                            (return :done)))
             (def fee foo)
             (def a (@fee 41))
             (def b (@fee 33))
             (def c (@fee 14 15))
             (return `(~a ~b ~c)))
           '(42 34 :done))

!(#testing @slash fntest
  (progn
    (defn cl-holder (x xs) (progn
                                    (return :nothing)
                                    (return x xs)))

    (defn cl-cons (x xs) (progn
                                    (def holder cl-holder)
                                    (@holder x xs)
                                    (return holder)))

    (defn cl-head (xs)
                  (progn
                    (def (h r) (xs @2 :nuthing))
                    (return h)))

    (defn cl-tail (xs)
                  (progn
                    (def (h r) (xs @2 :nuthin))
                    (return r)))

    (def cl-nil :nil)

    (def xs0 cl-nil)
    (def xs1 (cl-cons 42 xs0))
    (def xs2 (cl-cons 34 xs1))

    (def a (cl-head xs2))
    (def b (cl-head (cl-tail xs2)))
    (return `(~a ~b)))
  '(34 42))

!(#testing @slash fntest
  (progn
    (defn fff (x) (return (std @slash + x 42)))
    (def yyy (fff 1))
    (return yyy))
  43)

!(#testing @slash fntest
  (progn
    (defn fff ()
                   (progn
                     (def x 2)
                     (defn ggg ()
                                    (return (std @slash + x 40)))
                     (return ggg)))
    (def ggg-in-fff (@fff))
    (return (fff @slash ggg-in-fff)))
  42)

!(#testing @slash fntest
  (progn
    (libc @slash print 42)
    (return 33))
  33)

(defn print-all (xs)
   (if xs
       (progn
         (libc @slash print (std @slash head xs))
         (print-all (std @slash tail xs))
         (return '()))
     (return '())))

(if panics
    (progn
      (print-all panics)
      (std @slash panic "FAILED"))
  (progn))

(def x "if at the end of the module doesn't work well, so here is this statement:)")
