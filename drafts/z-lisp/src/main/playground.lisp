(def last
     (builtin.fn
	 (if (tail (head args))
	     (last (tail (head args)))
	   (head (head args)))))
(def progn (builtin.fn (last args)))
(def quote (builtin.operator  (head args)))

"hello, world!"

(def second
     (builtin.fn
      (head (tail (head args)))))

(def deconsfn
     (builtin.fn
      (if (head args)
	  `(progn
	     (def ~(head (head args)) (head ~(second args)))
	     ~(deconsfn (tail (head args)) `(tail ~(second args))))
	''())))

(def decons
     (builtin.macro
      (deconsfn (head args) (second args))))

(decons (a b c) '(7 8 9))
a
b
c

(def fn (builtin.macro `(builtin.fn (progn (decons ~(head args) args) ~(head (tail args))))))
(def macro (builtin.macro `(builtin.macro (progn (decons ~(head args) args) ~(head (tail args))))))
(def form (builtin.macro `(builtin.operator (progn (decons ~(head args) args) ~(head (tail args))))))

(def twice (fn (arg) (add arg arg)))
(twice 35)

(progn 1 2 3)
(def list (builtin.fn args))
(list 1 2 3)

(def defn (builtin.macro `(def ~(head args) ~(cons 'fn (tail args)))))
(def defmacro (builtin.macro `(def ~(head args) ~(cons 'macro (tail args)))))
(def defform (builtin.macro `(def ~(head args) ~(cons 'form (tail args)))))

(defmacro addpi (arg) (list 'add arg pi))
(def pi 3)
(addpi 8)

(if (list pi) pi (add pi pi))
(second '(1 2))
(defn third args (head (tail (tail (head args)))))

(defn map (f s)
  (if s
      (cons
       (f
	(head s))
       (map
	f
	(tail s)))
    '()))

(defn append (x xs)
  (if xs
      (cons
       (head xs)
       (append
	x
	(tail xs)))
    (list x)))

(map (fn (arg) (add arg arg)) '(1 2 3 4 5))
(append 5 '(1 2 3 4))

(defn adder (n) (fn (m) (add n m)))
((adder 3) 4)


(defmacro handle-error (name) `(def ~name (second ~name)))

(def libc (load-shared-library "libc.so.6"))
(handle-error libc)
	   
(def fopen
     (extern-pointer libc "fopen"
		     ((string string) pointer)))
(handle-error fopen)

(def malloc
     (extern-pointer libc "malloc"
		     ((sizet) pointer)))
(handle-error malloc)

(def fread
     (extern-pointer libc "fread"
		     ((pointer sizet sizet pointer) sizet)))
(handle-error fread)

(def printfptr
     (extern-pointer libc "printf"
		     ((string pointer) sizet)))
(handle-error printfptr)

(def hostsfile (fopen "/etc/hosts" "r"))
(def buffer (malloc 2048))
(fread buffer 1 1024 hostsfile)
(printfptr "%.2048s" buffer)

'(defn append (x ()) (list x) (x (cons head rest)) (cons head (append x rest)))
'(defn reverse  (()) (list)  ((cons head rest)) (append head (reverse rest)))

:foo
