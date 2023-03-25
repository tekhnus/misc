(req
 (prelude "prelude")
 (wrap-pointer-into-pointer "prelude" wrap-pointer-into-pointer)
 (dereference "prelude" dereference)
 (std "std")
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (repr "std" repr)
 (list-at "std" list-at)
 (panic "std" panic)
 (fprintf "libc" fprintf)
 (fprintf-bytestring "libc" fprintf-bytestring)
 (stdout "libc" stdout)
 (stderr "libc" stderr)
 (stdin "libc" stdin)
 (zlisp "zlisp")
 (comp-prg-new "zlisp" compile-prog-new)
 (iprog "zlisp" init-prog)
 (eval-new "zlisp" eval-new)
 (rd "zlisp" read)
 (repr-pointer "zlisp" repr-pointer)
 (mres "zlisp" make-routine-with-empty-state)
 (psm "zlisp" prog-slice-make)
 (psan "zlisp" prog-slice-append-new)
 (cdm "zlisp" compdata-make)
 (em "zlisp" ext-make))

!(req
  (stdmacro "stdmacro")
  (switch "stdmacro" switch))

(def readme "A basic REPL for zlisp.")

(defn repl
  (sl nsp pptr bpptr compdata bdrcompdata ex)
  (progn
    (def tmp (/prelude/fprintf stdout "> "))
    !(#/stdmacro/switch
      (/zlisp/rd stdin)
      (((:eof)
	(return (/prelude/fprintf stdout "\n")))
       ((:ok datum)
        (def maybe-prog (/zlisp/comp-prg-new sl pptr bpptr datum compdata bdrcompdata ex))
        !(#/stdmacro/switch
          maybe-prog
          (((:ok progxxx)
            !(#/stdmacro/switch
              (/zlisp/eval-new sl nsp)
              (((:ok val ctxt)
                (def ignored (/prelude/fprintf-bytestring
                           stdout "%s\n" (/zlisp/repr-pointer val)))
                (return (../repl sl ctxt pptr bpptr compdata bdrcompdata ex)))
               ((:err msg)
		(def ignored (/prelude/fprintf-bytestring stderr "eval error: %s\n" msg))
		(return (../repl sl nsp pptr bpptr compdata bdrcompdata ex))))))
           ((:err msg)
            (def ignored (/prelude/fprintf-bytestring
                       stderr "compilation error at repl: %s\n" msg))
            (return (../repl sl nsp pptr bpptr compdata bdrcompdata ex))))))
       ((:err msg)
	(def ignored (/prelude/fprintf-bytestring stderr "read error: %s\n" msg))
	(return (../repl sl nsp pptr bpptr compdata bdrcompdata ex)))))))

(def sl (/prelude/psm 20000))
(def pptr (/prelude/wrap-pointer-into-pointer (/zlisp/psan sl)))
(def bpptr (/prelude/wrap-pointer-into-pointer (/zlisp/psan sl)))
(def rt (/prelude/mres (/prelude/dereference bpptr 'int64) (/prelude/wrap-pointer-into-pointer 0)))
(def compdata (/prelude/cdm))
(def bdrcompdata (/prelude/cdm))
(def ex (/prelude/em))
(def xxx (/zlisp/iprog sl pptr bpptr compdata bdrcompdata))
(def ignored (repl sl rt pptr bpptr compdata bdrcompdata ex))
