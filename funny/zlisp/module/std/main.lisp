req
{{prelude "prelude"}
 {panic- "prelude" panic}
 {head- "prelude" head}
 {tail- "prelude" tail}
 {cons- "prelude" cons}
 {eq- "prelude" eq}
 {annotate- "prelude" annotate}
 {is-constant- "prelude" is-constant}
 {repr- "prelude" repr}
 {concat-bytestrings- "prelude" concat-bytestrings}
 {+- "prelude" +}}

panic := fn {x}
{(/prelude/panic- @0 x)
 {return {}}}

head := fn {x}
{return (/prelude/head- x)}

tail := fn {x}
{return (/prelude/tail- x)}

cons := fn {x xs}
{return (/prelude/cons- x xs)}

eq := fn {x y}
{return (/prelude/eq- x y)}

annotate := fn {x}
{return (/prelude/annotate- x)}

is-constant := fn {x}
{return (/prelude/is-constant- x)}

repr := fn {x}
{return (/prelude/repr- x)}

concat-bytestrings := fn {x y}
{return (/prelude/concat-bytestrings- x y)}

+ := fn {x y}
{return (/prelude/+- x y)}

not := fn {x}
{if x
 {return {list
   {}}}
 {return {list
   {{list
     {}}}}}}

last := 42

last = fn {a0}
{if (../tail a0)
 {return (../last (../tail a0))}
 {return (../head a0)}}

type := fn {x}
{return (../head (../annotate x))}

length := fn {x}
{n := 0
 while x
 {n = (../+ n 1)
  x = (../tail x)}
 {return n}}

concat := 42

concat = fn {a0 a1}
{if a0
 {return (../cons (../head a0) (../concat (../tail a0) a1))}
 {return a1}}

zip := 42

zip = fn {a0 a1}
{if a0
 {return (../cons {list
    {(../head a0)
     (../head a1)}} (../zip (../tail a0) (../tail a1)))}
 {return {list
   {}}}}

map := 42

map = fn {a0 a1}
{if a1
 {return (../cons (../a0 (../head a1)) (../map a0 (../tail a1)))}
 {return {list
   {}}}}

panic-block := {list
 {'argz
  '(/std/panic
   @0
   "wrong fn call")}}

list-at := 42

list-at = fn {xs n}
{if (../eq n 0)
 {{return (../head xs)}}
 {{return (../list-at (../tail xs) (../+ n -1))}}}

swtchone := 42

swtchone = fn {a0}
{if a0
 {firstarg := (../head a0)
  cond := (../head firstarg)
  body := (../list-at firstarg 1)
  rest := (../swtchone (../tail a0))
  return {list
   {'prearg
    '=
    cond
    'if
    '(/std/eq
     (/std/head prearg)
     :ok)
    {list
     {'args
      '=
      '(/std/list-at
       prearg
       1)
      body}}
    rest}}}
 {return '(/std/panic
   @0
   "nothing matched")}}

decons-pat := 42

decons-pat = fn {a0 a1}
{pat := a0
 val := a1
 if (../is-constant pat)
 {{if (../eq pat val)
   {return {list
     {':ok
      {list
       {}}}}}
   {return {list
     {':err}}}}}
 {if (../eq (../type pat) :symbol)
  {{return {list
     {:ok
      {list
       val}}}}}
  {if (../eq (../type pat) :list)
   {if pat
    {if val
     {first-decons := (../decons-pat (../head pat) (../head val))
      rest-decons := (../decons-pat (../tail pat) (../tail val))
      {if (../eq :err (../head rest-decons))
       {return {list
         {':err}}}
       {if (../eq :err (../head first-decons))
        {return {list
          {':err}}}
        {return {list
          {:ok
           (../concat (../list-at first-decons 1) (../list-at rest-decons 1))}}}}}}
     {{return {list
        {':err}}}}}
    {if val
     {{return {list
        {':err}}}}
     {{return {list
        {':ok
         {list
          {}}}}}}}}
   {(../panic @0 "decons-pat met an unsupported type")}}}}

decons-vars := 42

decons-vars = fn {a0}
{if (../is-constant a0)
 {return {list
   {}}}
 {if (../eq (../type a0) :symbol)
  {return {list
    {a0}}}
  {if (../eq (../type a0) :list)
   {if a0
    {return (../concat (../decons-vars (../head a0)) (../decons-vars (../tail a0)))}
    {return {list
      {}}}}
   (panic @0 "decons-var met an unsupported type")}}}

switch-defines := {list
 {'(/std/list-at
   args
   0)
  '(/std/list-at
   args
   1)
  '(/std/list-at
   args
   2)
  '(/std/list-at
   args
   3)
  '(/std/list-at
   args
   4)
  '(/std/list-at
   args
   5)}}

make-assignment := fn {x}
{return {list
  {(../head x)
   ':=
   (../list-at x 1)}}}

switch-clause := fn {a0}
{{a1 := a0}
 sig := (../head a1)
 {if sig
  {}
  {{} := (../panic "empty signature")}}
 cmds := (../tail a1)
 quoted-sig := {list
  {'quote
   sig}}
 checker := {list
  {'call
   {list
    {'/std/decons-pat
     quoted-sig
     'args}}}}
 vars := (../decons-vars sig)
 body := (../concat (../map make-assignment (../zip vars switch-defines)) cmds)
 {return {list
   {checker
    body}}}}

switch-fun := fn {a0}
{return (../swtchone (../map switch-clause a0))}

append := 42

append = fn {x xs}
{if xs
 {return (../cons (../head xs) (../append x (../tail xs)))}
 {return {list
   {x}}}}

first-good-value := 42

first-good-value = fn {x}
{if x
 {first-arg := (../head x)
  {if (../eq :ok (../head first-arg))
   {{return (../list-at first-arg 1)}}
   {{return (../first-good-value (../tail x))}}}}
 (panic @0 "first-good-value: no good value")}

export
{{panic panic}
 {head head}
 {tail tail}
 {cons cons}
 {eq eq}
 {annotate annotate}
 {is-constant is-constant}
 {repr repr}
 {concat-bytestrings concat-bytestrings}
 {+ +}
 {length length}
 {decons-pat decons-pat}
 {append append}
 {list-at list-at}
 {switch-fun switch-fun}
 {first-good-value first-good-value}
 {type type}
 {not not}}
