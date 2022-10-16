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
 (comp-prg-new "zlisp" compile-prog-new)
 (ev "zlisp" eval)
 (eval-new "zlisp" eval-new)
 (rd "zlisp" read)
 (mres "zlisp" make-routine-with-empty-state)
 (dec "zlisp" decode-offset)
 (bt "zlisp" builtins)
 (psm "zlisp" prog-slice-make)
 (psan "zlisp" prog-slice-append-new)
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
  (def prog (dec nsp))
  !(#switchx (rd stdin)
	  ((:eof)
	   (return (fprintf stdout "\n")))
	  ((:ok datum)
           (def maybe-prog (comp-prg-new sl prog datum compdata))
           !(#switchx maybe-prog
                      ((:ok progxxx)
                       !(#switchx (eval-new sl nsp)
		                  ((:ok val ctxt)
		                   !(#ignore (fprintf-bytestring stdout "%s\n" (repr val)))
		                   (return (repl sl ctxt compdata)))
		                  ((:err msg)
		                   !(#ignore (fprintf-bytestring stderr "eval error: %s\n" msg))
		                   (return (repl sl nsp compdata)))))
                      ((:err msg)
                       !(#ignore (fprintf-bytestring stderr "compilation error at repl: %s\n" msg))
                       (return (repl sl nsp compdata)))))
	  ((:err msg)
	   !(#ignore (fprintf-bytestring stderr "read error: %s\n" msg))
	   (return (repl sl nsp compdata)))))

(def builtins_ (bt))
(def sl (psm 20000))
(def p (psan sl))
(def rt (mres p))
(def compdata (cdm))

!(#ignore (repl sl rt compdata))

(export)
