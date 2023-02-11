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

!(#/testing/fntest
  (return "hello, world!")
  "hello, world!")

!(#/testing/fntest
  (return (std/+ 4 3))
  7)

!(#/testing/fntest
  (return (std/list-at '(1 2) 1))
  2)

!(#/testing/fntest
  (return (std/eq :foo :bar))
  '())

!(#/testing/fntest
  (progn
    (def bar :foo)
    (return (std/eq :foo bar)))
  '(()))

!(#/testing/fntest
  (progn
    (return (std/append 5 '(1 2 3 4))))
  '(1 2 3 4 5))

!(#/testing/fntest
  (return `(1 2 ~(std/+ 1 2)))
  '(1 2 3))

!(#/testing/fntest
  (progn
    (defn twice (arg) (return (std/+ arg arg)))
    (return (twice 35)))
  70)

!(#/testing/fntest
  (progn
    (defn adderf (n)
      (progn
        (def m (return :ready))
        (return (std/+ n m))))
    (defn adder (n)
      (progn
        (def a adderf)
        (a @mut n)
        (return a)))
    (return ((adder 3) 4)))
  7)

!(#/testing/fntest
  (progn
    (defn fib () (progn
                   (return 3)
                   (return 5)
                   (return 8)
                   (return 13)))
    (def (x) (fib @mut))
    (def (y) (fib @mut '()))
    (def (z) (fib @mut '()))
    (def (t) (fib @mut '()))
    (return `(~x ~y ~z ~t)))
  '(3 5 8 13))

!(#/testing/fntest
  (progn
    (defn far-fib () (progn
                       (return @event-loop 3)
                       (return @event-loop 5)
                       (return 8)))

    (defn more-far-fib () (progn
                            (def x (far-fib))
                            (return @event-loop x)
                            (return @event-loop 13)))

    (def (x) (more-far-fib @mut @event-loop))
    (def (y) (more-far-fib @mut @event-loop '()))
    (def (z) (more-far-fib @mut @event-loop '()))
    (def (t) (more-far-fib @mut @event-loop '()))
    (return `(~x ~y ~z ~t)))
  '(3 5 8 13))

!(#/testing/fntest
  (progn
    (prelude/fprintf stderr "hello")
    (return 42))
  42)


!(#/testing/fntest
  (progn
    (defn multi-ret () (return 42 34))
    (def (x y) (multi-ret @2))
    (return `(~x ~y)))
  '(42 34))

!(#/testing/fntest
  (progn
    (defn foo (x) (progn
                    (def y (return (std/+ x 1)))
                    (def (z t) (return @2 (std/+ y 1)))
                    (return :done)))
    (def fee foo)
    (def a (fee @mut 41))
    (def b (fee @mut 33))
    (def c (fee @mut 14 15))
    (return `(~a ~b ~c)))
  '(42 34 :done))

!(#/testing/fntest
  (progn
    (defn cl-holder (x xs) (progn
                             (return :nothing)
                             (return x xs)))

    (defn cl-cons (x xs) (progn
                           (def holder cl-holder)
                           (holder @mut x xs)
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

!(#/testing/fntest
  (progn
    (defn fff (x) (return (std/+ x 42)))
    (def yyy (fff 1))
    (return yyy))
  43)

!(#/testing/fntest
  (progn
    (defn fff ()
      (progn
        (def x 2)
        (defn ggg ()
          (return (std/+ x 40)))
        (return ggg)))
    (def ggg-in-fff (fff @mut))
    (return (fff/ggg-in-fff)))
  42)

!(#/testing/fntest
  (progn
    (libc/print 42)
    (return 33))
  33)

!(#/testing/fntest
  (progn
    (defn do-something (x)
      (progn
        (libc/print x)
        (return 'do-something-value)))
    (defn interceptor (arg)
      (progn
        (def (ext-pointer arg) (do-something @mut @(host "call-extension-1") @2 arg))
        (libc/print "extension:")
        (libc/print ext-pointer)
        (libc/print "argument:")
        (libc/print arg)
        (def host-res (return @(host "call-extension") ext-pointer arg))
        (interceptor @something host-res)))
    (def res (interceptor 'arg))
    (return res))
  'do-something-value)

(defn print-all (xs)
  (if xs
      (progn
        (libc/print (std/head xs))
        (print-all (std/tail xs))
        (return '()))
    (return '())))

(if panics
    (progn
      (print-all panics)
      (std/panic "FAILED"))
  (progn))

(def x "if at the end of the module doesn't work well, so here is this statement:)")
