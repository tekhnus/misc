(req
 (std "std")
 (fprintf "libc" fprintf)
 (fprintf-bytestring "libc" fprintf-bytestring)
 (stdout "libc" stdout)
 (stdin "libc" stdin)
 (comp-prg "zlisp" compile-prog)
 (ev "zlisp" eval)
 (rd "zlisp" read)
 (bt "zlisp" builtins)
 (psm "zlisp" prog-slice-make))

!(req (stdmacro "stdmacro"))
!(importall stdmacro)

(importall std)

(def readme "A basic REPL for zlisp.")

!(#defun repl
  (sl nsp)
  (def tmp (fprintf stdout "> "))
  !(#switchx (rd stdin)
	  ((:eof)
	   (return (fprintf stdout "\n")))
	  ((:ok datum)
           (def maybe-prog (comp-prg sl datum))
           !(#switchx maybe-prog
                      ((:ok prog)
                       !(#switchx (ev sl nsp prog)
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

(def builtins_ (bt))
(def sl (psm 20000))

!(#ignore (repl sl builtins_))

(export)
