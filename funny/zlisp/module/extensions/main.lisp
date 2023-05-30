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
 {tail- "std" tail}
 {cons- "std" cons}
 {testing "testing"}
 {fntest- "testing" fntestx}}

.switch := fn {exp argz}
#{return #{'flat
  #{'args
   '=
   exp
   (/std/switch-fun argz)}}}

.switch.arity := 3

.fntest := fn {body expect}
#{return #{'list
  (/testing/fntest- body expect)}}

.fntest.arity := 3

.backquote := 42

.backquote = fn {exp}
#{if (/std/not- (/std/eq- (/std/type- exp) :list))
 #{return #{'quote
   exp}}
 #{if (/std/not- exp)
  #{return #{'list
    #{}}}
  #{if (/std/eq- (/std/length- exp) 2)
   #{if (/std/eq- (/std/list-at- exp 0) 'tilde)
    #{return (/std/list-at- exp 1)}
    #{return #{'call
      #{'/std/cons
       (../.backquote (/std/head- exp))
       (../.backquote (/std/tail- exp))}}}}
   #{return #{'call
     #{'/std/cons
      (../.backquote (/std/head- exp))
      (../.backquote (/std/tail- exp))}}}}}}

.backquote.arity := 2

.defnx := fn {name args body}
#{src := #{name
  ':=
  'fn
  '{}
  #{'list
   #{'mc
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
  #{'call
   #{name
    '@mut
    '@0}}}
 return #{'flat
  src}}

.defnx.arity := 4
