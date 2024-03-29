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

panics := {}

val := 0

calltest := 0

fntest
{return (std/head {'42
   '5
   '3})}
42

fntest
{return (std/tail {'42
   '5
   '3})}
{'5
 '3}

fntest
{return (std/head (std/tail {'42
    '5
    '3}))}
5

fntest
{return (std/list-at {'42
   '5
   '3} 1)}
5

fntest
{return "hello, world!"}
"hello, world!"

fntest
{return (std/+ 4 3)}
7

fntest
{return {(/std/+ 4 3)
  8}}
{'7
 '8}

fntest
{return (std/list-at {'1
   '2} 1)}
2

fntest
{return (std/eq :foo :bar)}
{}

fntest
{bar := :foo
 return (std/eq :foo bar)}
{{}}

fntest
{return (std/append 5 {'1
   '2
   '3
   '4})}
{'1
 '2
 '3
 '4
 '5}

fntest
{twice := fn {arg}
 {return (std/+ arg arg)}
 return (twice 35)}
70

fntest
{adderf := fn {n}
 {m := return @1
  ^{}
  return (std/+ n m)}
 adder := fn {n}
 {a := adderf
  {} := (a @0 @mut n)
  return a}
 return ((adder 3) 4)}
7

fntest
{fib := fn {}
 {return 3
  return 5
  return 8
  return 13}
 {x} := (fib @mut)
 {y} := (fib @mut)
 {z} := (fib @mut)
 {t} := (fib @mut)
 return {x
  y
  z
  t}}
{'3
 '5
 '8
 '13}

fntest
{fff := fn {x}
 {return (std/+ x 42)}
 yyy := (fff 1)
 return yyy}
43

fntest
{multi-ret := fn {}
 {return ^{42
   34}}
 {x y} := (multi-ret @2)
 return {x
  y}}
{'42
 '34}

fntest
{y := 3
 fff := fn {}
 {x := 2
  ggg := fn {}
  {return (std/+ x 40)}
  return ggg}
 ggg-in-fff := (fff @mut)
 return (fff/ggg-in-fff)}
42

print-all := 42

print-all = fn {xs}
{res := 42
 if xs
 {res = (/prelude/print (/std/head xs))
  {} := (../print-all @0 (/std/tail xs))
  return ^{}}
 {return ^{}}}

if panics

{{} := (print-all @0 panics)
 {} := (/std/panic @0 "FAILED")}

{}
