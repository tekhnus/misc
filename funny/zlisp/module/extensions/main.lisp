req
{{std "std"}
 {switch-fun "std" switch-fun}
 {not- "std" not}
 {eq- "std" eq}
 {type- "std" type}
 {panic- "std" panic}
 {length- "std" length}
 {head- "std" head}
 {list-at- "std" list-at}
 {append- "std" append}
 {tail- "std" tail}
 {cons- "std" cons}
 {testing "testing"}
 {fntest- "testing" fntestx}}

.switch := fn {exp argz}
{return {'args
  '=
  exp
  (/std/switch-fun argz)}}

.switch.arity := 3

.fntest := fn {body expect}
{return (/testing/fntest- body expect)}

.fntest.arity := 3

.backquotex := 44

.backquotex = fn {exp}
{if (/std/not- (/std/eq- (/std/type- exp) :list))
 {return {'quote
   exp}}
 {if (/std/not- exp)
  {return {}}
  {if (/std/eq- (/std/length- exp) 2)
   {if (/std/eq- (/std/list-at- exp 0) 'tilde)
    {return (/std/list-at- exp 1)}
    {return {'call
      {'/std/cons
       (../.backquotex (/std/head- exp))
       (../.backquotex (/std/tail- exp))}}}}
   {return {'call
     {'/std/cons
      (../.backquotex (/std/head- exp))
      (../.backquotex (/std/tail- exp))}}}}}}

.backquote2 := 43

.backquote2 = fn {exp}
{res := {}
 ignored := 0
 item := 0
 val := 0
 while (exp)
 ^{item = (/std/head- exp)
  ignored = if (/std/eq- item 'tilde)
  {exp = (/std/tail- exp)
   val = (/std/head- exp)
   res = (/std/append- val res)}
  {res = (/std/append- item res)}
  exp = (/std/tail- exp)}
 return res}

.backquote := fn {exp}
{return {(../.backquotex (/std/list-at- exp 0))}}

.backquote2 = fn {exp}
{return (../.backquote2 exp)}

.backquote.arity := 2

.defnx := fn {name args body}
{src := {name
  ':=
  'fn
  '{}
  {{'mc
    ':=
    'magically_called_fn
    args
    body
    '{}
    ':=
    '(mc
     @mut
     @0
     @up2)
    'return
    :shouldnt-go-here}}
  '{}
  ':=
  {'call
   {name
    '@mut
    '@0}}}
 return src}

.defnx.arity := 4
