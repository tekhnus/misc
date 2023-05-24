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
{return (std/head {list {'42 '5 '3}} )}
42

fntest
{return (std/tail {list {'42 '5 '3}} )}
{list
 {'5
  '3}}

fntest
{return (std/head (std/tail {list {'42 '5 '3}} ) )}
5

fntest
{return (std/list-at {list {'42 '5 '3}} 1 )}
5

fntest
{return "hello, world!"}
"hello, world!"

fntest
{return (std/+ 4 3 )}
7

fntest
{return {list {(/std/+ 4 3 ) 8}}}
{list
 {'7
  '8}}

fntest
{return (std/list-at {list {'1 '2}} 1 )}
2

fntest
{return (std/eq :foo :bar )}
{list
 {}}

fntest
{bar = :foo
 return (std/eq :foo bar )}
{list
 {{list
   {}}}}

fntest
{return (std/append 5 {list {'1 '2 '3 '4}} )}
{list
 {'1
  '2
  '3
  '4
  '5}}

fntest
{defn twice {arg}
 {return (std/+ arg arg )}
 return (twice 35 )}
70

fntest
{defn adderf {n}
 {m = {return @1 {}}
  return (std/+ n m )}
 defn adder {n}
 {a = adderf
  {} = (a @0 @mut n )
  return a}
 return ((adder 3 ) 4 )}
7

fntest
{defn fib {}
 {return 3
  return 5
  return 8
  return 13}
 {x} = (fib @mut )
 {y} = (fib @mut )
 {z} = (fib @mut )
 {t} = (fib @mut )
 return {list {x y z t}}}
{list
 {'3
  '5
  '8
  '13}}

fntest
{defn fff {x}
 {return (std/+ x 42 )}
 yyy = (fff 1 )
 return yyy}
43

fntest
{defn multi-ret {}
 {return {42 34}}
 {x y} = (multi-ret @2 )
 return {list {x y}}}
{list
 {'42
  '34}}

fntest
{y = 3
 defn fff {}
 {x = 2
  defn ggg {}
  {return (std/+ x 40 )}
  return ggg}
 ggg-in-fff = (fff @mut )
 return (fff/ggg-in-fff )}
42

defn print-all {xs}
{if xs
 {res = (/prelude/print (/std/head xs ) )
  {} = (../print-all @0 (/std/tail xs ) )
  return {}}
 {return {}}}

if panics
{{} = (print-all @0 panics )
 {} = (/std/panic @0 "FAILED" )
 x = 42}

{x = 33}
