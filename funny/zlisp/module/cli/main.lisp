(req
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (repr "std" repr)
 (second "std" second)
 (third "std" third)
 (panic "std" panic)
 (fprintf "libc" fprintf)
 (fprintf-bytestring "libc" fprintf-bytestring)
 (stdout "libc" stdout)
 (stderr "libc" stderr)
 (stdin "libc" stdin)
 (comp-prg "zlisp" compile-prog)
 (ev "zlisp" eval)
 (rd "zlisp" read)
 (bt "zlisp" builtins)
 (psm "zlisp" prog-slice-make)
 (cdm "zlisp" compdata-make))

!(req
  (ignore "std" ignore))

!(req
  (defun "stdmacro" defun)
  (fn "stdmacro" fn)
  (def2 "stdmacro" def2)
  (switchx "stdmacro" switchx))

(def readme "A basic REPL for zlisp.")

!(#defun repl
  (sl nsp compdata)
  (def tmp (fprintf stdout "> "))
  !(#switchx (rd stdin)
	  ((:eof)
	   (return (fprintf stdout "\n")))
	  ((:ok datum)
           (def maybe-prog (comp-prg sl datum compdata))
           !(#switchx maybe-prog
                      ((:ok prog)
                       !(#switchx (ev sl nsp prog)
		                  ((:ok val ctxt)
		                   !(#ignore (fprintf-bytestring stdout "%s\n" (repr val)))
		                   (return (repl sl ctxt compdata)))
		                  ((:err msg)
		                   !(#ignore (fprintf-bytestring stderr "eval error: %s\n" msg))
		                   (return (repl sl nsp compdata)))))
                      ((:err msg)
                       !(#ignore (fprintf-bytestring stderr "compilation error: %s\n" msg))
                       (return (repl sl nsp compdata)))))
	  ((:err msg)
	   !(#ignore (fprintf-bytestring stderr "read error: %s\n" msg))
	   (return (repl sl nsp compdata)))))

(def builtins_ (bt))
(def sl (psm 20000))
(def compdata (cdm))

!(#ignore (repl sl builtins_ compdata))

(export)
