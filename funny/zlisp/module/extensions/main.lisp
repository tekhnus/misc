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

.backquote := 43

.backquote = fn {exp}
{res := {}
 ignored := 0
 item := 0
 val := 0
 while exp
 ^{item = (/std/head- exp)
  ignored = if (/std/eq- item 'tilde)
  {exp = (/std/tail- exp)
   val = (/std/head- exp)
   val = (/std/head- val)
   res = (/std/append- val res)}
  {if (/std/eq- (/std/type- item) :list) {
res = (/std/append- (../.backquote item) res)
} {
   res = (/std/append- {'quote item} res)}}
  exp = (/std/tail- exp)}
 return res}

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
