(defn repl
  (nsp)
  (fprintf stdout "> ")
  (def readres (read stdin))
  (switch readres
	  ((:eof)
	   (fprintf stdout "\n"))
	  ((:ok datum)
	   (def v (eval-in nsp datum))
	   (switch v
		   ((:ok val)
		    (print val))
		   ((:err msg)
		    (fprintf-bytestring stderr "eval error: %s\n" msg)))
	   (repl nsp))
	  ((:err msg)
	   (fprintf-bytestring stderr "read error: %s\n" msg)
	   (repl nsp))))

(def ns (make-namespace))
(repl ns)
