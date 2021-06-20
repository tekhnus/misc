(def last
     (builtin.fn
	 (if (tail (head args))
	     (last (tail (head args)))
	   (head (head args)))))
(def progn (builtin.fn (last args)))
(def quote (builtin.form  (head args)))

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
(def macro (builtin.macro `(builtin.macro (progn (def ~(head args) args) ~(head (tail args))))))
(def form (builtin.macro `(builtin.form (progn (def ~(head args) args) ~(head (tail args))))))

(def twice (fn (arg) (add arg arg)))
(twice 35)

(progn 1 2 3)
(def list (builtin.fn args))
(list 1 2 3)

(def defn (macro args `(def ~(head args) ~(cons 'fn (tail args)))))
(def defmacro (macro args `(def ~(head args) ~(cons 'macro (tail args)))))
(def defform (macro args `(def ~(head args) ~(cons 'form (tail args)))))

(defmacro addpi args (list 'add (head args) pi))
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

(def fopen
     (extern-pointer "libSystem.dylib" "fopen"
		((string string) pointer)))
(def malloc
     (extern-pointer "libSystem.dylib" "malloc"
		((sizet) pointer)))
(def fread
     (extern-pointer "libSystem.dylib" "fread"
		  ((pointer sizet sizet pointer) sizet)))

(def feof
     (extern-pointer "libSystem.dylib" "feof"
		((pointer) int)))

(def printfptr
     (extern-pointer "libSystem.dylib" "printf"
		((string pointer) sizet)))

(def fprintfstring
     (extern-pointer "libSystem.dylib" "fprintf"
		((pointer string string) sizet)))



(def stdin
     (extern-pointer "libSystem.dylib" "__stdinp" pointer))

(def stdout
     (extern-pointer "libSystem.dylib" "__stdoutp" pointer))

(def stderr
     (extern-pointer "libSystem.dylib" "__stderrp" pointer))

(def hostsfile (fopen "/etc/hosts" "r"))
(def buffer (malloc 2048))
(fread buffer 1 1024 hostsfile)
'(printfptr "%.2048s" buffer)
'(fprintfstring stdout "%s" "Hello, world!")
'(feof stdin)

'(defn append (x ()) (list x) (x (cons head rest)) (cons head (append x rest)))
'(defn reverse  (()) (list)  ((cons head rest)) (append head (reverse rest)))

:foo
