(print "hello, world!")

(def args (list :p :q :r))

!(#defun twice (arg) (+ arg arg))

(print (twice 35))

(print (list 1 2 3))

(def pi 3)


(print (if (list pi) pi (add pi pi)))
(print (second '(1 2)))
!(#defun third args (head (tail (tail (head args)))))

(print (append 5 '(1 2 3 4)))

'!(#defun adder (n) (fn (m) (+ n m)))
'(print ((adder 3) 4))

!(#def-or-panica printfptr
     (extern-pointer libc "printf"
		     '((string pointer) sizet)))

(def hostsfile (fopen "/etc/hosts" "r"))
(def buffer (malloc 2048))
(ignore (fread buffer 1 1024 hostsfile))
(ignore (printfptr "%.2048s" buffer))

(def foo :foo)

(print (eq :foo :bar))
(print (eq :foo foo))
