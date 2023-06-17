req
{{prelude "prelude"}
 {wrap-pointer-into-pointer "prelude" wrap-pointer-into-pointer}
 {dereference "prelude" dereference}
 {std "std"}
 {decons-pat "std" decons-pat}
 {eq "std" eq}
 {head "std" head}
 {repr "std" repr}
 {list-at "std" list-at}
 {panic "std" panic}
 {fprintf "libc" fprintf}
 {libc "libc"}
 {print "libc" print}
 {stdout "libc" stdout}
 {stderr "libc" stderr}
 {stdin "libc" stdin}
 {zlisp "zlisp"}
 {comp-prg-new "zlisp" compile-prog-new}
 {iprog "zlisp" init-prog}
 {eval-new "zlisp" eval-new}
 {rd "zlisp" read}
 {repr-pointer "zlisp" repr-pointer}
 {mres "zlisp" make-routine-with-empty-state}
 {psm "zlisp" prog-slice-make}
 {cdm "zlisp" compdata-make}
 {em "zlisp" ext-make}}

args := 0

prearg := 0

comment := "this is a workaround for switch"

readme := "A basic REPL for zlisp."

repl := 42

repl = fn {sl nsp bpptr compdata bdrcompdata ex}
{tmp := (/prelude/fprintf stdout "> ")
 ignored := 0
 maybe-prog := 42
 datum := 42
 progxxx := 42
 val := 42
 msga := 42
 msgb := 42
 msgc := 42
 switch (/zlisp/rd stdin)
 {{{:eof}
   return (/prelude/fprintf stdout "")}
  {{:ok
    datum}
   maybe-prog = (/zlisp/comp-prg-new sl bpptr datum compdata bdrcompdata ex)
   switch maybe-prog
   {{{:ok
      progxxx}
     switch (/zlisp/eval-new sl nsp)
     {{{:ok
        val}
       ignored = (/libc/print (/zlisp/repr-pointer val))
       return (../repl sl nsp bpptr compdata bdrcompdata ex)}
      {{:err
        msga}
       ignored = (/libc/print "eval error\n")
       ignored = (/libc/print msga)
       return (../repl sl nsp bpptr compdata bdrcompdata ex)}}}
    {{:err
      msgb}
     ignored = (/libc/print "compilation error at repl\n")
     ignored = (/libc/print msgb)
     return (../repl sl nsp bpptr compdata bdrcompdata ex)}}}
  {{:err
    msgc}
   ignored = (/libc/print "read error\n")
   ignored = (/libc/print msgc)
   return (../repl sl nsp bpptr compdata bdrcompdata ex)}}}

sl := (/prelude/psm)

bpptr := (/prelude/wrap-pointer-into-pointer 0)

rt := (/prelude/mres (/prelude/dereference bpptr 'int64) (/prelude/wrap-pointer-into-pointer 0))

compdata := (/prelude/cdm)

bdrcompdata := (/prelude/cdm)

ex := (/prelude/em)

bpval := (/zlisp/iprog sl compdata bdrcompdata)

bpptr = (/prelude/wrap-pointer-into-pointer bpval)

ignored := (repl sl rt bpptr compdata bdrcompdata ex)
