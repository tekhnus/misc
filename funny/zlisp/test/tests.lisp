req
{{prelude "prelude"}
 {fprintf "libc" fprintf}
 {stderr "libc" stderr}
 {std "std"}
 {decons-pat "std" decons-pat}
 {eq "std" eq}
 {head "std" head}
 {repr "std" repr}
 {append "std" append}
 {length "std" length}
 {list-at "std" list-at}
 {type "std" type}
 {+ "std" +}
 {panic "std" panic}
 {concat-bytestrings "std" concat-bytestrings}
 {cons "std" cons}
 {tail "std" tail}
 {panic "std" panic}
 {not "std" not}
 {libc "libc"}
 {print "libc" print}}

panics = {list {}}
fntest
{return "hello, world!"}
"hello, world!"

fntest
{return {call {/std/+ 4 3}}}
7

fntest
{return {call {/std/+ 4 3}}}
7

fntest
{return {call {/std/length {list {'1 '2 '3}}}}}
3

fntest
{return {call {/std/length {list {}}}}}
0

fntest
{return {call {/std/type {list {}}}}}
:list

fntest
{return `42}
42

fntest
{return `{42 {call {/std/+ 33 1}}}}
{list
 {'brackets
  '42
  {list
   {'brackets
    'call
    {list
     {'brackets
      '/std/+
      '33
      '1}}}}}}

fntest
{return `{42 ~{call {/std/+ 33 1}}}}
{list
 {'brackets
  '42
  '34}}

fntest
{return {list {{call {/std/+ 4 3}} 8}}}
{list
 {'7
  '8}}

fntest
{return {call {/std/list-at {list {'1 '2}} 1}}}
2

fntest
{return {call {/std/eq :foo :bar}}}
{list
 {}}

fntest
{bar = :foo
 {return {call {/std/eq :foo bar}}}}
{list
 {{list
   {}}}}

fntest
{return {call {/std/append 5 {list {'1 '2 '3 '4}}}}}
{list
 {'1
  '2
  '3
  '4
  '5}}

fntest
{defn twice {arg}
 {return {call {/std/+ arg arg}}}
 {return {call {twice 35}}}}
70

fntest
{defnx twice {arg}
 {return {call {/std/+ arg arg}}}
 {return {call {twice 35}}}}
70

fntest
{defnx twice {arg}
 {return {call {/std/+ arg arg}}}
 defn four-times {arg}
 {return {call {/std/+ {call {../twice arg}} {call {../twice arg}}}}}
 {return {call {four-times 35}}}}
140

fntest
{defn fib {n}
 {if {call {/std/eq n 0}}
  {return 0}
  {if {call {/std/eq n 1}}
   {return 1}
   {return {call {/std/+ {call {../fib {call {/std/+ n -1}}}} {call {../fib {call {/std/+ n -2}}}}}}}}}
 return {call {fib 5}}}
5

fntest
{x = 0
 y = 1
 while {call {/std/not {call {/std/eq x 5}}}}
 {y = {call {/std/+ y y}}
  x = {call {/std/+ x 1}}}
 {return y}}
32

fntest
{defn adderf {n}
 {m = {return @1 {}}
  {return {call {/std/+ n m}}}}
 defn adder {n}
 {a = adderf
  {} = {call {../a @0 @mut n}}
  {return a}}
 {return {call {{call {adder 3}} 4}}}}
7

fntest
{defn fib {}
 {{return 3}
  {return 5}
  {return 8}
  {return 13}}
 {x} = {call {fib @mut}}
 {y} = {call {fib @mut}}
 {z} = {call {fib @mut}}
 {t} = {call {fib @mut}}
 {return {list {x y z t}}}}
{list
 {'3
  '5
  '8
  '13}}

fntest
{defn far-fib {}
 {{return @event-loop
   3}
  {return @event-loop
   5}
  {return 8}}
 defn more-far-fib {}
 {x = {call {../far-fib}}
  {return @event-loop
   x}
  {return @event-loop
   13}}
 {x} = {call {more-far-fib @mut @event-loop}}
 {y} = {call {more-far-fib @mut @event-loop}}
 {z} = {call {more-far-fib @mut @event-loop}}
 {t} = {call {more-far-fib @mut @event-loop}}
 {return {list {x y z t}}}}
{list
 {'3
  '5
  '8
  '13}}

fntest
{res = {call {/prelude/fprintf stderr "hello"}}
 {return 42}}
42

fntest
{defn multi-ret {}
 {{} = {return {42 34}}}
 {x y} = {call {multi-ret @2}}
 {return {list {x y}}}}
{list
 {'42
  '34}}

fntest
{defn foo {x}
 {y = {return @1 {call {/std/+ x 1}}}
  {z t} = {return @2 {call {/std/+ y 1}}}
  {return :done}}
 fee = foo
 a = {call {fee @mut 41}}
 b = {call {fee @mut 33}}
 c = {call {fee @mut 14 15}}
 {return {list {a b c}}}}
{list
 {'42
  '34
  ':done}}

fntest
{defn cl-holder {x xs}
 {{} = {return {}}
  {} = {return {x xs}}}
 defn cl-cons {x xs}
 {holder = cl-holder
  {} = {call {../holder @0 @mut x xs}}
  {return holder}}
 defn cl-head {xs}
 {{h r} = {call {../xs @2}}
  {return h}}
 defn cl-tail {xs}
 {{h r} = {call {../xs @2}}
  {return r}}
 cl-nil = :nil
 xs0 = cl-nil
 xs1 = {call {cl-cons 42 xs0}}
 xs2 = {call {cl-cons 34 xs1}}
 a = {call {cl-head xs2}}
 b = {call {cl-head {call {cl-tail xs2}}}}
 {return {list {a b}}}}
{list
 {'34
  '42}}

fntest
{defn fff {x}
 {return {call {/std/+ x 42}}}
 yyy = {call {fff 1}}
 {return yyy}}
43

fntest
{defn fff {}
 {x = 2
  defn ggg {}
  {return {call {/std/+ x 40}}}
  {return ggg}}
 ggg-in-fff = {call {fff @mut}}
 {return {call {fff/ggg-in-fff}}}}
42

fntest
{res = {call {/libc/print 42}}
 {return 33}}
33

fntest
{defn do-something {x}
 {res = {call {/libc/print x}}
  {return 'do-something-value}}
 defn interceptor {arg}
 {{ext-pointer arg-} = {call {../do-something @mut @{host "call-extension-1"} @2 arg}}
  res = {call {/libc/print "extension:"}}
  res = {call {/libc/print ext-pointer}}
  res = {call {/libc/print "argument:"}}
  res = {call {/libc/print arg-}}
  host-res = {return @1 @{host "call-extension"} {ext-pointer arg-}}
  {} = {call {../interceptor @0 @something host-res}}}
 res = {call {interceptor 'arg}}
 {return res}}
'do-something-value

fntest
{defn wrapper {}
 {defn __magically_called__ {x}
  {return {list {x x}}}
  {} = {call {__magically_called__ @mut @0 @up}}
  {return 33}}
 {} = {call {wrapper @mut @0}}
 res = {call {wrapper 42}}
 {return res}}
{list
 {'42
  '42}}

fntest
{if 3
 {return 42}
 {return 25}}
42

fntest
{x = -5
 while {call {/std/not {call {/std/eq x 0}}}}
 {x = {call {/std/+ x 1}}}
 defn f {x}
 {return x}
 if 3
 {return 42}
 {return 25}}
42

fntest
{a = 5
 {return a}}
5

defn print-all {xs}
{if xs
 {res = {call {/libc/print {call {/std/head xs}}}}
  {} = {call {../print-all @0 {call {/std/tail xs}}}}
  return {}}
 {{} = {return {}}}}

if panics
{{} = {call {print-all @0 panics}}
 {} = {call {/std/panic @0 "FAILED"}}}

{}

x = "if at the end of the module doesn't work well, so here is this statement:)"
