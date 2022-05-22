(req
 (std "std")
 (libc "libc")
 (zlisp "zlisp"))

!(req (stdmacro "stdmacro"))
!(importall stdmacro)

(importall std)
(importall libc)
(importall zlisp)

(def readme "A basic REPL for zlisp.")

!(#defun repl
  (sl nsp)
  (def tmp (fprintf stdout "> "))
  !(#switchx (read stdin)
	  ((:eof)
	   (return (fprintf stdout "\n")))
	  ((:ok datum)
           (def maybe-prog (compile-prog sl datum))
           !(#switchx maybe-prog
                      ((:ok prog)
                       !(#switchx (eval sl nsp prog)
		                  ((:ok val ctxt)
		                   !(#ignore (fprintf-bytestring stdout "%s\n" (repr val)))
		                   (return (repl sl ctxt)))
		                  ((:err msg)
		                   !(#ignore (fprintf-bytestring stderr "eval error: %s\n" msg))
		                   (return (repl sl nsp)))))
                      ((:err msg)
                       !(#ignore (fprintf-bytestring stderr "compilation error: %s\n" msg))
                       (return (repl sl nsp)))))
	  ((:err msg)
	   !(#ignore (fprintf-bytestring stderr "read error: %s\n" msg))
	   (return (repl sl nsp)))))

(def builtins_ (builtins))
(def sl (prog-slice-make 20000))

!(#ignore (repl sl builtins_))
