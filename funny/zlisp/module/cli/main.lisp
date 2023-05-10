req
{(prelude
  "prelude")
 (wrap-pointer-into-pointer
  "prelude"
  wrap-pointer-into-pointer)
 (dereference
  "prelude"
  dereference)
 (std
  "std")
 (decons-pat
  "std"
  decons-pat)
 (eq
  "std"
  eq)
 (head
  "std"
  head)
 (repr
  "std"
  repr)
 (list-at
  "std"
  list-at)
 (panic
  "std"
  panic)
 (fprintf
  "libc"
  fprintf)
 (fprintf-bytestring
  "libc"
  fprintf-bytestring)
 (stdout
  "libc"
  stdout)
 (stderr
  "libc"
  stderr)
 (stdin
  "libc"
  stdin)
 (zlisp
  "zlisp")
 (comp-prg-new
  "zlisp"
  compile-prog-new)
 (iprog
  "zlisp"
  init-prog)
 (eval-new
  "zlisp"
  eval-new)
 (rd
  "zlisp"
  read)
 (repr-pointer
  "zlisp"
  repr-pointer)
 (mres
  "zlisp"
  make-routine-with-empty-state)
 (psm
  "zlisp"
  prog-slice-make)
 (psan
  "zlisp"
  prog-slice-append-new)
 (cdm
  "zlisp"
  compdata-make)
 (em
  "zlisp"
  ext-make)}

readme = "A basic REPL for zlisp."
defn repl (sl nsp pptr bpptr compdata bdrcompdata ex)
{tmp = (/prelude/fprintf stdout "> ")
 switch
 (/zlisp/rd
  stdin)
 (((:eof)
   {return (/prelude/fprintf stdout "
")})
  ((:ok
    datum)
   maybe-prog = (/zlisp/comp-prg-new sl pptr bpptr datum compdata bdrcompdata ex)
   switch
   maybe-prog
   (((:ok
      progxxx)
     switch
     (/zlisp/eval-new
      sl
      nsp)
     (((:ok
        val
        ctxt)
       ignored = (/prelude/fprintf-bytestring stdout "%s
" (/zlisp/repr-pointer val))
       {return (../repl sl ctxt pptr bpptr compdata bdrcompdata ex)})
      ((:err
        msg)
       ignored = (/prelude/fprintf-bytestring stderr "eval error: %s
" msg)
       {return (../repl sl nsp pptr bpptr compdata bdrcompdata ex)})))
    ((:err
      msg)
     ignored = (/prelude/fprintf-bytestring stderr "compilation error at repl: %s
" msg)
     {return (../repl sl nsp pptr bpptr compdata bdrcompdata ex)})))
  ((:err
    msg)
   ignored = (/prelude/fprintf-bytestring stderr "read error: %s
" msg)
   {return (../repl sl nsp pptr bpptr compdata bdrcompdata ex)}))}

sl = (/prelude/psm 20000)
pptr = (/prelude/wrap-pointer-into-pointer (/zlisp/psan sl))
bpptr = (/prelude/wrap-pointer-into-pointer (/zlisp/psan sl))
rt = (/prelude/mres (/prelude/dereference bpptr 'int64) (/prelude/wrap-pointer-into-pointer 0))
compdata = (/prelude/cdm)
bdrcompdata = (/prelude/cdm)
ex = (/prelude/em)
xxx = (/zlisp/iprog sl pptr bpptr compdata bdrcompdata)
ignored = (repl sl rt pptr bpptr compdata bdrcompdata ex)
