(req
 (fprintf "libc" fprintf)
 (stderr "libc" stderr)
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (repr "std" repr)
 (second "std" second)
 (append "std" append)
 (+ "std" +)
 (panic "std" panic)
 (concat-bytestrings "std" concat-bytestrings)
 (cons "std" cons)
 (tail "std" tail)
 (panic "std" panic)
 (print "libc" print))

!(req (fntest "testing" fntest))

(def panics '())

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
    (builtin.defn adder (n) (return (builtin.fn (m) (return (+ n m)))))
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
    (builtin.defn far-fib () (progn
       (^return 3)
       (^return 5)
       (return 8)))

    (builtin.defn more-far-fib () (progn
       (def x (far-fib))
       (^return x)
       (^return 13)))

    (def (x) (@^more-far-fib))
    (def (y) (@^more-far-fib '()))
    (def (z) (@^more-far-fib '()))
    (def (t) (@^more-far-fib '()))
    (return `(~x ~y ~z ~t)))
  '(3 5 8 13))

!(#fntest
  (progn
    (fprintf stderr "hello")
    (return 42))
  42)


!(#fntest
  (progn
    (builtin.defn multi-ret () (return 42 34))
    (def (x y) (multi-ret @2))
    (return `(~x ~y)))
  '(42 34))

!(#fntest
  (progn
    (builtin.defn cl-cons (x xs)
                  (return (builtin.fn () (return x xs))))

    (builtin.defn cl-head (xs)
                  (progn
                    (def (h r) (xs @2))
                    (return h)))

    (builtin.defn cl-tail (xs)
                  (progn
                    (def (h r) (xs @2))
                    (return r)))

    (def cl-nil :nil)

    (def xs0 cl-nil)
    (def xs1 (cl-cons 42 xs0))
    (def xs2 (cl-cons 34 xs1))

    (def a (cl-head xs2))
    (def b (cl-head (cl-tail xs2)))
    (return `(~a ~b)))
  '(34 42))

!(#fntest
  (progn
    (builtin.defun fff (x) (return (+ x 42)))
    (def yyy (fff 1))
    (return yyy))
  43)

!(#fntest
  (progn
    (builtin.defun fff ()
                   (progn
                     (def x 2)
                     (builtin.defun ggg ()
                                    (return (+ x 40)))
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
  (progn))

(def x "if at the end of the module doesn't work well, so here is this statement:)")
