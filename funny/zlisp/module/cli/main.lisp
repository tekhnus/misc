req
[[prelude "prelude"]
 [wrap-pointer-into-pointer "prelude" wrap-pointer-into-pointer]
 [dereference "prelude" dereference]
 [std "std"]
 [decons-pat "std" decons-pat]
 [eq "std" eq]
 [head "std" head]
 [repr "std" repr]
 [list-at "std" list-at]
 [panic "std" panic]
 [fprintf "libc" fprintf]
 [fprintf-bytestring "libc" fprintf-bytestring]
 [stdout "libc" stdout]
 [stderr "libc" stderr]
 [stdin "libc" stdin]
 [zlisp "zlisp"]
 [comp-prg-new "zlisp" compile-prog-new]
 [iprog "zlisp" init-prog]
 [eval-new "zlisp" eval-new]
 [rd "zlisp" read]
 [repr-pointer "zlisp" repr-pointer]
 [mres "zlisp" make-routine-with-empty-state]
 [psm "zlisp" prog-slice-make]
 [cdm "zlisp" compdata-make]
 [em "zlisp" ext-make]]

readme = "A basic REPL for zlisp."
defn repl [sl nsp bpptr compdata bdrcompdata ex]
[tmp = (/prelude/fprintf stdout "> ")
 switch (/zlisp/rd stdin)
 [[[:eof]
   [return (/prelude/fprintf stdout "")]]
  [[:ok
    datum]
   maybe-prog = (/zlisp/comp-prg-new sl bpptr datum compdata bdrcompdata ex)
   switch maybe-prog
   [[[:ok
      progxxx]
     switch (/zlisp/eval-new sl nsp)
     [[[:ok
        val]
       ignored = (/prelude/fprintf-bytestring stdout "%s\n" (/zlisp/repr-pointer val))
       [return (../repl sl nsp bpptr compdata bdrcompdata ex)]]
      [[:err
        msg]
       ignored = (/prelude/fprintf-bytestring stderr "eval error: %s\n" msg)
       [return (../repl sl nsp bpptr compdata bdrcompdata ex)]]]]
    [[:err
      msg]
     ignored = (/prelude/fprintf-bytestring stderr "compilation error at repl: %s\n" msg)
     [return (../repl sl nsp bpptr compdata bdrcompdata ex)]]]]
  [[:err
    msg]
   ignored = (/prelude/fprintf-bytestring stderr "read error: %s\n" msg)
   [return (../repl sl nsp bpptr compdata bdrcompdata ex)]]]]

sl = (/prelude/psm)
bpptr = (/prelude/wrap-pointer-into-pointer 0)
rt = (/prelude/mres (/prelude/dereference bpptr 'int64) (/prelude/wrap-pointer-into-pointer 0))
compdata = (/prelude/cdm)
bdrcompdata = (/prelude/cdm)
ex = (/prelude/em)
bpval = (/zlisp/iprog sl compdata bdrcompdata)
bpptr = (/prelude/wrap-pointer-into-pointer bpval)
ignored = (repl sl rt bpptr compdata bdrcompdata ex)
