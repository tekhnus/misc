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
{if (/std/not- (/std/eq- (/std/head- argz ) 'brackets ) )
 {return "expected brackets"}
 {}
 {return {list {'brackets 'args '= exp (/std/switch-fun (/std/tail- argz ) )}}}}

.switch.arity = 3
defn .fntest {body expect}
{return (/testing/fntest- body expect )}

.fntest.arity = 3
defn .backquote {exp}
{if (/std/not- (/std/eq- (/std/type- exp ) :list ) )
 {return {list {'brackets 'quote exp}}}
 {if (/std/not- exp )
  {return {list {'brackets 'list {list {'brackets}}}}}
  {if (/std/eq- (/std/length- exp ) 3 )
   {if (/std/eq- (/std/list-at- exp 1 ) 'tilde )
    {return (/std/list-at- exp 2 )}
    {return {list {'brackets 'call {list {'brackets '/std/cons (../.backquote (/std/head- exp ) ) (../.backquote (/std/tail- exp ) )}}}}}}
   {return {list {'brackets 'call {list {'brackets '/std/cons (../.backquote (/std/head- exp ) ) (../.backquote (/std/tail- exp ) )}}}}}}}}

.backquote.arity = 2
defn .defnx {name args body}
{return {list {'brackets 'defn name {list 'brackets} {list {'brackets 'defn '__magically_called__ args body {list 'brackets} '= '(__magically_called__ @mut @0 @up2) 'return :shouldnt-go-here}} {list 'brackets} '= {list {'brackets 'call {list {'brackets name '@mut '@0}}}}}}}

.defnx.arity = 4
