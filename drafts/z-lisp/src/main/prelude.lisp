(def last
     (builtinfn
	 (if (cdr (car args))
	     (last (cdr (car args)))
	   (car (car args)))))
(def progn (builtinfn (last args)))
(def quote (builtinform  (car args)))

"hello, world!"

(def second
     (builtinfn
      (car (cdr (car args)))))
(def head car)
(def tail cdr)

(def deconsfn
     (builtinfn
      (if (head args)
	  `(progn
	     (def ~(head (head args)) (head ~(second args)))
	     ~(deconsfn (tail (head args)) `(tail ~(second args))))
	''())))

(def decons
     (builtinmacro
      (deconsfn (head args) (second args))))

(decons (a b c) '(7 8 9))
a
b
c

(def fn (builtinmacro `(builtinfn (progn (decons ~(car args) args) ~(car (cdr args))))))
(def macro (builtinmacro `(builtinmacro (progn (def ~(car args) args) ~(car (cdr args))))))
(def form (builtinmacro `(builtinform (progn (def ~(car args) args) ~(car (cdr args))))))

(def twice (fn (arg) (add arg arg)))
(twice 35)

(progn 1 2 3)
(def list (builtinfn args))
(list 1 2 3)

(def defn (macro args `(def ~(car args) ~(cons 'fn (cdr args)))))
(def defmacro (macro args `(def ~(car args) ~(cons 'macro (cdr args)))))
(def defform (macro args `(def ~(car args) ~(cons 'form (cdr args)))))

(defmacro addpi args (list 'add (car args) pi))
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
     (externcdata "libSystem.dylib" "fopen"
		((string string) pointer)))
(def malloc
     (externcdata "libSystem.dylib" "malloc"
		((sizet) pointer)))
(def fread
     (externcdata "libSystem.dylib" "fread"
		  ((pointer sizet sizet pointer) sizet)))

(def feof
     (externcdata "libSystem.dylib" "feof"
		((pointer) int)))

(def printfptr
     (externcdata "libSystem.dylib" "printf"
		((string pointer) sizet)))

(def fprintfstring
     (externcdata "libSystem.dylib" "fprintf"
		((pointer string string) sizet)))



(def stdin
     (externcdata "libSystem.dylib" "__stdinp" pointer))

(def stdout
     (externcdata "libSystem.dylib" "__stdoutp" pointer))

(def stderr
     (externcdata "libSystem.dylib" "__stderrp" pointer))

(def hostsfile (fopen "/etc/hosts" "r"))
(def buffer (malloc 2048))
(fread buffer 1 1024 hostsfile)
(printfptr "%.2048s" buffer)
(fprintfstring stdout "%s" "Hello, world!")
(feof stdin)

'(defn append (x ()) (list x) (x (cons head rest)) (cons head (append x rest)))
'(defn reverse  (()) (list)  ((cons head rest)) (append head (reverse rest)))

(print (eval (read stdin)))
