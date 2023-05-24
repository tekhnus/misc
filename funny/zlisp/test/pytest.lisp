req
{{std "std"}
 {decons-pat "std" decons-pat}
 {eq "std" eq}
 {head "std" head}
 {tail "std" tail}
 {repr "std" repr}
 {append "std" append}
 {list-at "std" list-at}
 {+ "std" +}
 {panic "std" panic}
 {cons "std" cons}
 {prelude "prelude"}
 {print "prelude" print}
 {concat-bytestrings "std" concat-bytestrings}}

panics = {list {}}
fntest
{return {call {std/head {list {'42 '5 '3}}}}}
42

fntest
{return {call {std/tail {list {'42 '5 '3}}}}}
{list
 {'5
  '3}}

fntest
{return {call {std/head {call {std/tail {list {'42 '5 '3}}}}}}}
5

fntest
{return {call {std/list-at {list {'42 '5 '3}} 1}}}
5

fntest
{return "hello, world!"}
"hello, world!"

fntest
{return {call {std/+ 4 3}}}
7

fntest
{return {list {{call {/std/+ 4 3}} 8}}}
{list
 {'7
  '8}}

fntest
{return {call {std/list-at {list {'1 '2}} 1}}}
2

fntest
{return {call {std/eq :foo :bar}}}
{list
 {}}

fntest
{bar = :foo
 return {call {std/eq :foo bar}}}
{list
 {{list
   {}}}}

fntest
{return {call {std/append 5 {list {'1 '2 '3 '4}}}}}
{list
 {'1
  '2
  '3
  '4
  '5}}

fntest
{defn twice {arg}
 {return {call {std/+ arg arg}}}
 return {call {twice 35}}}
70

fntest
{defn adderf {n}
 {m = {return @1 {}}
  return {call {std/+ n m}}}
 defn adder {n}
 {a = adderf
  {} = {call {a @0 @mut n}}
  return a}
 return {call {{call {adder 3}} 4}}}
7

fntest
{defn fib {}
 {return 3
  return 5
  return 8
  return 13}
 {x} = {call {fib @mut}}
 {y} = {call {fib @mut}}
 {z} = {call {fib @mut}}
 {t} = {call {fib @mut}}
 return {list {x y z t}}}
{list
 {'3
  '5
  '8
  '13}}

fntest
{defn fff {x}
 {return {call {std/+ x 42}}}
 yyy = {call {fff 1}}
 return yyy}
43

fntest
{defn multi-ret {}
 {return {42 34}}
 {x y} = {call {multi-ret @2}}
 return {list {x y}}}
{list
 {'42
  '34}}

fntest
{y = 3
 defn fff {}
 {x = 2
  defn ggg {}
  {return {call {std/+ x 40}}}
  return ggg}
 ggg-in-fff = {call {fff @mut}}
 return {call {fff/ggg-in-fff}}}
42

defn print-all {xs}
{if xs
 {res = {call {/prelude/print {call {/std/head xs}}}}
  {} = {call {../print-all @0 {call {/std/tail xs}}}}
  return {}}
 {return {}}}

if panics
{{} = {call {print-all @0 panics}}
 {} = {call {/std/panic @0 "FAILED"}}
 x = 42}

{x = 33}
