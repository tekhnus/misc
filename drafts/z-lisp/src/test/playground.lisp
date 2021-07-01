(print "hello, world!")

(def args (list :p :q :r))

(print (switch 42
	    (33
	     :one)
	    (42
	     :two)))

(def twice (fn (arg) (+ arg arg)))
(print (twice 35))

(print (list 1 2 3))

(def pi 3)
(defmacro addpi (arg) (list '+ arg pi))
(print (addpi 8))

(print (if (list pi) pi (add pi pi)))
(print (second '(1 2)))
(defn third args (head (tail (tail (head args)))))

(print (map (fn (arg) (+ arg arg)) '(1 2 3 4 5)))
(print (append 5 '(1 2 3 4)))

(defn adder (n) (fn (m) (+ n m)))
(print ((adder 3) 4))


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
(ignore (fread buffer 1 1024 hostsfile))
(ignore (printfptr "%.2048s" buffer))

(def foo :foo)

(print (eq :foo :bar))
(print (eq :foo foo))
