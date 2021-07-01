"hello, world!"

(print args)

(def args (list :p :q :r))
(code-block
 (a b c)
 777
 )

(switch 42
	    (33
	     :one)
	    (42
	     :two))

(decons (a b c) '(7 8 9))
a
b
c

(def twice (fn (arg) (add arg arg)))
(twice 35)

(progn 1 2 3)
(list 1 2 3)

(defmacro addpi (arg) (list 'add arg pi))
(def pi 3)
(addpi 8)

(if (list pi) pi (add pi pi))
(second '(1 2))
(defn third args (head (tail (tail (head args)))))

(map (fn (arg) (add arg arg)) '(1 2 3 4 5))
(append 5 '(1 2 3 4))

(defn adder (n) (fn (m) (add n m)))
((adder 3) 4)


(def fopen
     (extern-pointer libc "fopen"
		     '((string string) pointer)))
(handle-error fopen)


(def printfptr
     (extern-pointer libc "printf"
		     '((string pointer) sizet)))
(handle-error printfptr)

(def hostsfile (fopen "/etc/hosts" "r"))
(def buffer (malloc 2048))
(fread buffer 1 1024 hostsfile)
(printfptr "%.2048s" buffer)

'(defn append (x ()) (list x) (x (cons head rest)) (cons head (append x rest)))
'(defn reverse  (()) (list)  ((cons head rest)) (append head (reverse rest)))

(def foo :foo)

(eq :foo :bar)
(eq :foo foo)
