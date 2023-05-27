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

defn .switch {exp argz}
{return {list {'args ':= exp (/std/switch-fun argz)}}}

.switch.arity := 3
defn .fntest {body expect}
{return (/testing/fntest- body expect)}

.fntest.arity := 3
defn .backquote {exp}
{if (/std/not- (/std/eq- (/std/type- exp) :list))
 {return {list {'quote exp}}}
 {if (/std/not- exp)
  {return {list {'list {list {}}}}}
  {if (/std/eq- (/std/length- exp) 2)
   {if (/std/eq- (/std/list-at- exp 0) 'tilde)
    {return (/std/list-at- exp 1)}
    {return {list {'call {list {'/std/cons (../.backquote (/std/head- exp)) (../.backquote (/std/tail- exp))}}}}}}
   {return {list {'call {list {'/std/cons (../.backquote (/std/head- exp)) (../.backquote (/std/tail- exp))}}}}}}}}

.backquote.arity := 2
defn .defnx {name args body}
{return {list {'defn name '{} {list {'defn '__magically_called__ args body '{} ':= '(__magically_called__ @mut @0 @up2) 'return :shouldnt-go-here}} '{} ':= {list {'call {list {name '@mut '@0}}}}}}}

.defnx.arity := 4
