req
{(prelude
  "prelude")
 (panic-
  "prelude"
  panic)
 (head-
  "prelude"
  head)
 (tail-
  "prelude"
  tail)
 (cons-
  "prelude"
  cons)
 (eq-
  "prelude"
  eq)
 (annotate-
  "prelude"
  annotate)
 (is-constant-
  "prelude"
  is-constant)
 (repr-
  "prelude"
  repr)
 (concat-bytestrings-
  "prelude"
  concat-bytestrings)
 (+-
  "prelude"
  +)}

defn panic (x)
{(/prelude/panic-
  @0
  x)
 {return {}}}

defn head (x)
{return (/prelude/head- x)}

defn tail (x)
{return (/prelude/tail- x)}

defn cons (x xs)
{return (/prelude/cons- x xs)}

defn eq (x y)
{return (/prelude/eq- x y)}

defn annotate (x)
{return (/prelude/annotate- x)}

defn is-constant (x)
{return (/prelude/is-constant- x)}

defn repr (x)
{return (/prelude/repr- x)}

defn concat-bytestrings (x y)
{return (/prelude/concat-bytestrings- x y)}

defn + (x y)
{return (/prelude/+- x y)}

defn last (a0)
{if (../tail a0)
 {return (../last (../tail a0))}
 {return (../head a0)}}

defn type (x)
{return (../head (../annotate x))}

defn length (x)
{n = 0
 while
 x
 {n = (../+ n 1)
  x = (../tail x)}
 {return n}}

defn concat (a0 a1)
{if a0
 {return (../cons (../head a0) (../concat (../tail a0) a1))}
 {return a1}}

defn zip (a0 a1)
{if a0
 {return (../cons {list {(../head a0) (../head a1)}} (../zip (../tail a0) (../tail a1)))}
 {return '()}}

defn map (a0 a1)
{if a1
 {return (../cons (../a0 (../head a1)) (../map a0 (../tail a1)))}
 {return '()}}

panic-block = '(argz (/std/panic @0 "wrong fn call"))
defn list-at (xs n)
{if (../eq n 0)
 {{return (../head xs)}}
 {{return (../list-at (../tail xs) (../+ n -1))}}}

defn swtchone (a0)
{if a0
 {firstarg = (../head a0)
  cond = (../head firstarg)
  body = (../list-at firstarg 1)
  rest = (../swtchone (../tail a0))
  {return {list {'brackets 'prearg '= cond 'if '(/std/eq (/std/head prearg) :ok) {list {'brackets 'args '= '(/std/list-at prearg 1) body}} rest}}}}
 {firstarg = "ifhack"
  cond = "ifhack"
  body = "ifhack"
  rest = "ifhack"
  {return '(/std/panic @0 "nothing matched")}}}

defn decons-pat (a0 a1)
{pat = a0
 val = a1
 if (../is-constant pat)
 {first-decons = "ifhack"
  rest-decons = "ifhack"
  {if (../eq pat val)
   {return '(:ok ())}
   {return '(:err)}}}
 {if (../eq (../type pat) :symbol)
  {first-decons = "ifhack"
   rest-decons = "ifhack"
   {return {list {:ok {list val}}}}}
  {if (../eq (../type pat) :list)
   {if pat
    {if val
     {first-decons = (../decons-pat (../head pat) (../head val))
      rest-decons = (../decons-pat (../tail pat) (../tail val))
      {if (../eq :err (../head rest-decons))
       {return '(:err)}
       {if (../eq :err (../head first-decons))
        {return '(:err)}
        {return {list {:ok (../concat (../list-at first-decons 1) (../list-at rest-decons 1))}}}}}}
     {first-decons = "ifhack"
      rest-decons = "ifhack"
      {return '(:err)}}}
    {if val
     {first-decons = "ifhack"
      rest-decons = "ifhack"
      {return '(:err)}}
     {first-decons = "ifhack"
      rest-decons = "ifhack"
      {return '(:ok ())}}}}
   {first-decons = "ifhack"
    rest-decons = "ifhack"
    (../panic
     @0
     "decons-pat met an unsupported type")}}}}

defn decons-vars (a0)
{if (../is-constant a0)
 {return '()}
 {if (../eq (../type a0) :symbol)
  {return {list {a0}}}
  {if (../eq (../type a0) :list)
   {if a0
    {return (../concat (../decons-vars (../head a0)) (../decons-vars (../tail a0)))}
    {return '()}}
   (panic
    @0
    "decons-var met an unsupported type")}}}

switch-defines = '((/std/list-at args 0) (/std/list-at args 1) (/std/list-at args 2) (/std/list-at args 3) (/std/list-at args 4) (/std/list-at args 5))
defn make-assignment (x)
{return {list {'brackets (../head x) '= (../list-at x 1)}}}

defn switch-clause (a0)
{sig = (../head a0)
 cmds = (../tail a0)
 checker = {list {'/std/decons-pat {list {'brackets 'quote sig}} 'args}}
 vars = (../decons-vars sig)
 body = (../cons 'brackets (../concat (../map make-assignment (../zip vars switch-defines)) cmds))
 {return {list {checker body}}}}

defn switch-fun (a0)
{return (../swtchone (../map switch-clause a0))}

defn append (x xs)
{if xs
 {return (../cons (../head xs) (../append x (../tail xs)))}
 {return {list {x}}}}

defn first-good-value (x)
{if x
 {first-arg = (../head x)
  {if (../eq :ok (../head first-arg))
   {{return (../list-at first-arg 1)}}
   {{return (../first-good-value (../tail x))}}}}
 (panic
  @0
  "first-good-value: no good value")}

defn not (x)
{if x
 {return '()}
 {return '(())}}

export

{(panic
  panic)
 (head
  head)
 (tail
  tail)
 (cons
  cons)
 (eq
  eq)
 (eq
  eq)
 (annotate
  annotate)
 (is-constant
  is-constant)
 (repr
  repr)
 (concat-bytestrings
  concat-bytestrings)
 (+
  +)
 (length
  length)
 (decons-pat
  decons-pat)
 (append
  append)
 (list-at
  list-at)
 (switch-fun
  switch-fun)
 (first-good-value
  first-good-value)
 (type
  type)
 (not
  not)}
