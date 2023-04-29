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
 (length "std" length)
 (list-at "std" list-at)
 (type "std" type)
 (+ "std" +)
 (panic "std" panic)
 (concat-bytestrings "std" concat-bytestrings)
 (cons "std" cons)
 (tail "std" tail)
 (panic "std" panic)
 (not "std" not)
 (libc "libc")
 (print "libc" print))

(def panics '())

(fntest
  (return "hello, world!")
  "hello, world!")

(fntest
  (return (/std/+ 4 3))
  7)

(fntest
 (return (/std/length '(1 2 3)))
 3)

(fntest
 (return (/std/length '()))
 0)

(fntest
 (return (/std/type '()))
 :list)

(fntest
 (return (backquote 42))
 42)

(fntest
 (return (backquote (42 33)))
 '(42 33))

(fntest
 (return (backquote (42 33 55)))
 '(42 33 55))

(fntest
 (return (backquote ()))
 '())

(fntest
 (return (backquote (42 (33 1))))
 '(42 (33 1)))

(fntest
 (return (backquote (42 (/std/+ 33 1))))
 '(42 (/std/+ 33 1)))

(fntest
 (return (backquote (42 ~(/std/+ 33 1))))
 '(42 34))

(fntest
 (return (backquote (42 ~(list (/std/+ 33 1) (backquote foo)))))
 '(42 (34 foo)))

(fntest
  (return (list (/std/+ 4 3) 8))
  '(7 8))

(fntest
  (return (/std/list-at '(1 2) 1))
  2)

(fntest
  (return (/std/eq :foo :bar))
  '())

(fntest
  (progn
    (def bar :foo)
    (return (/std/eq :foo bar)))
  '(()))

(fntest
  (progn
    (return (/std/append 5 '(1 2 3 4))))
  '(1 2 3 4 5))

(fntest
  (return `(1 2 ~(/std/+ 1 2)))
  '(1 2 3))

(fntest
  (progn
    (defn twice (arg) (return (/std/+ arg arg)))
    (return (twice 35)))
  70)

(fntest
  (progn
    (defn2 twice (arg) (return (/std/+ arg arg)))
    (return (twice 35)))
  70)

(fntest
  (progn
    (defn2 twice (arg) (return (/std/+ arg arg)))
    (defn four-times (arg) (return (/std/+ (../twice arg) (../twice arg))))
    (return (four-times 35)))
  140)

(fntest
  (progn
    (def x 0)
    (def y 1)
    (while (/std/not (/std/eq x 5))
      (progn
        (def y (/std/+ y y))
        (def x (/std/+ x 1))))
    (return y))
  32)

(fntest
  (progn
    (defn adderf (n)
      (progn
        (def m (return @1 :ready))
        (return (/std/+ n m))))
    (defn adder (n)
      (progn
        (def a adderf)
        (../a @mut n)
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
    (defn far-fib () (progn
                       (return @0 @event-loop 3)
                       (return @0 @event-loop 5)
                       (return @0 8)))

    (defn more-far-fib () (progn
                            (def x (../far-fib))
                            (return @0 @event-loop x)
                            (return @0 @event-loop 13)))

    (def (x) (more-far-fib @mut @event-loop))
    (def (y) (more-far-fib @mut @event-loop))
    (def (z) (more-far-fib @mut @event-loop))
    (def (t) (more-far-fib @mut @event-loop))
    (return `(~x ~y ~z ~t)))
  '(3 5 8 13))

(fntest
  (progn
    (/prelude/fprintf stderr "hello")
    (return 42))
  42)


(fntest
  (progn
    (defn multi-ret () (return 42 34))
    (def (x y) (multi-ret @2))
    (return `(~x ~y)))
  '(42 34))

(fntest
  (progn
    (defn foo (x) (progn
                    (def y (return @1 (/std/+ x 1)))
                    (def (z t) (return @2 (/std/+ y 1)))
                    (return :done)))
    (def fee foo)
    (def a (fee @mut 41))
    (def b (fee @mut 33))
    (def c (fee @mut 14 15))
    (return `(~a ~b ~c)))
  '(42 34 :done))

(fntest
  (progn
    (defn cl-holder (x xs) (progn
                             (return @1 :nothing)
                             (return @1 x xs)))

    (defn cl-cons (x xs) (progn
                           (def holder cl-holder)
                           (../holder @mut x xs)
                           (return holder)))

    (defn cl-head (xs)
      (progn
        (def (h r) (../xs @2 :nuthing))
        (return h)))

    (defn cl-tail (xs)
      (progn
        (def (h r) (../xs @2 :nuthin))
        (return r)))

    (def cl-nil :nil)

    (def xs0 cl-nil)
    (def xs1 (cl-cons 42 xs0))
    (def xs2 (cl-cons 34 xs1))

    (def a (cl-head xs2))
    (def b (cl-head (cl-tail xs2)))
    (return `(~a ~b)))
  '(34 42))

(fntest
  (progn
    (defn fff (x) (return (/std/+ x 42)))
    (def yyy (fff 1))
    (return yyy))
  43)

(fntest
  (progn
    (defn fff ()
      (progn
        (def x 2)
        (defn ggg ()
          (return (/std/+ x 40)))
        (return ggg)))
    (def ggg-in-fff (fff @mut))
    (return (fff/ggg-in-fff)))
  42)

(fntest
  (progn
    (/libc/print 42)
    (return 33))
  33)

(fntest
  (progn
    (defn do-something (x)
      (progn
        (/libc/print x)
        (return 'do-something-value)))
    (defn interceptor (arg)
      (progn
        (def (ext-pointer arg-) (../do-something @mut @(host "call-extension-1") @2 arg))
        (/libc/print "extension:")
        (/libc/print ext-pointer)
        (/libc/print "argument:")
        (/libc/print arg-)
        (def host-res (return @1 @(host "call-extension") ext-pointer arg-))
        (../interceptor @something host-res)))
    (def res (interceptor 'arg))
    (return res))
  'do-something-value)

(fntest
  (progn
    (defn wrapper ()
      (progn
        (defn wrapped (x)
          (progn
            (return `(~x ~x))))
        (wrapped @mut @pre @0 @up)
        (return 33)))
    (wrapper @mut @0)
    (def res (wrapper 42))
    (return res))
  '(42 42))

(fntest
 (progn
   (if 3 {(return 42)} {(return 25)}))
 42)

(fntest {
        (a = 5)
        (return a)}
 5)

(defn print-all (xs)
  (if xs
      (progn
        (/libc/print (/std/head xs))
        (../print-all (/std/tail xs))
        (return '()))
    (return '())))

(if panics
    (progn
      (print-all panics)
      (/std/panic "FAILED"))
  (progn))

(def x "if at the end of the module doesn't work well, so here is this statement:)")
