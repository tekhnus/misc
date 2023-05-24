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
 {fprintf-bytestring "libc" fprintf-bytestring}
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

readme = "A basic REPL for zlisp."
defn repl {sl nsp bpptr compdata bdrcompdata ex}
{tmp = {call {/prelude/fprintf stdout "> "}}
 switch {call {/zlisp/rd stdin}}
 {{{:eof}
   {return {call {/prelude/fprintf stdout ""}}}}
  {{:ok
    datum}
   maybe-prog = {call {/zlisp/comp-prg-new sl bpptr datum compdata bdrcompdata ex}}
   switch maybe-prog
   {{{:ok
      progxxx}
     switch {call {/zlisp/eval-new sl nsp}}
     {{{:ok
        val
        ctxt}
       ignored = {call {/prelude/fprintf-bytestring stdout "%s\n" {call {/zlisp/repr-pointer val}}}}
       {return {call {../repl sl ctxt bpptr compdata bdrcompdata ex}}}}
      {{:err
        msg}
       ignored = {call {/prelude/fprintf-bytestring stderr "eval error: %s\n" msg}}
       {return {call {../repl sl nsp bpptr compdata bdrcompdata ex}}}}}}
    {{:err
      msg}
     ignored = {call {/prelude/fprintf-bytestring stderr "compilation error at repl: %s\n" msg}}
     {return {call {../repl sl nsp bpptr compdata bdrcompdata ex}}}}}}
  {{:err
    msg}
   ignored = {call {/prelude/fprintf-bytestring stderr "read error: %s\n" msg}}
   {return {call {../repl sl nsp bpptr compdata bdrcompdata ex}}}}}}

sl = {call {/prelude/psm}}
bpptr = {call {/prelude/wrap-pointer-into-pointer 0}}
rt = {call {/prelude/mres {call {/prelude/dereference bpptr 'int64}} {call {/prelude/wrap-pointer-into-pointer 0}}}}
compdata = {call {/prelude/cdm}}
bdrcompdata = {call {/prelude/cdm}}
ex = {call {/prelude/em}}
bpval = {call {/zlisp/iprog sl compdata bdrcompdata}}
bpptr = {call {/prelude/wrap-pointer-into-pointer bpval}}
ignored = {call {repl sl rt bpptr compdata bdrcompdata ex}}
