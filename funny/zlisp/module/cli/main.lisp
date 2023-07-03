req
{{prelude "prelude"}
 {std "std"}
 {decons-pat "std" decons-pat}
 {eq "std" eq}
 {head "std" head}
 {list-at "std" list-at}
 {panic "std" panic}
 {fprintf-new "libc" fprintf-new}
 {fprintf-pointer-new "libc" fprintf-pointer-new}
 {libc "libc"}
 {print "libc" print}
 {stdin-val "libc" stdin-val}
 {stdout-val "libc" stdout-val}
 {zlisp "zlisp"}
 {comp-prg-new "zlisp" compile-prog-new}
 {iprog "zlisp" init-prog}
 {eval-new "zlisp" eval-new}
 {rd "zlisp" read}
 {repr-pointer "zlisp" repr-pointer}
 {mres "zlisp" make-routine-with-empty-state}
 {psm "zlisp" prog-slice-make}
 {cdm "zlisp" compdata-make}
 {cm "zlisp" context-make}
 {panic-if-aborted "zlisp" panic-if-aborted}
 {em "zlisp" ext-make}}

args := 0

prearg := 0

comment := "this is a workaround for switch"

readme := "A basic REPL for zlisp."

repl := 42

repl = fn {sl nsp bpptr compdata bdrcompdata ex}
{tmp := (/prelude/fprintf-new stdout-val "> ")
 ignored := 0
 maybe-prog := 42
 datum := 42
 progxxx := 42
 val := 42
 msga := 42
 msgb := 42
 msgc := 42
 read-res := (/zlisp/rd stdin-val)
 switch read-res
 {{{:eof}
   return (/prelude/fprintf-new stdout-val "")}
  {{:ok
    datum}
   maybe-prog = (/zlisp/comp-prg-new sl bpptr datum compdata bdrcompdata ex)
   switch maybe-prog
   {{{:ok
      progxxx}
     switch (/zlisp/eval-new sl nsp)
     {{{:ok
        val}
       ignored = (/prelude/fprintf-pointer-new stdout-val (/zlisp/repr-pointer val))
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

rt := (/prelude/mres)

compdata := (/prelude/cdm)

bdrcompdata := (/prelude/cdm)

ctxt := (/prelude/cm)

ex := (/prelude/em ctxt)

{} := (/zlisp/panic-if-aborted @0 ctxt)

bpptr := (/zlisp/iprog sl compdata bdrcompdata)

ignored := (repl sl rt bpptr compdata bdrcompdata ex)
