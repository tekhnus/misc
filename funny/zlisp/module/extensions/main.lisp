req
{{std "std"}
 {switch-fun "std" switch-fun}
 {not- "std" not}
 {eq- "std" eq}
 {type- "std" type}
 {length- "std" length}
 {head- "std" head}
 {list-at- "std" list-at}
 {tail- "std" tail}
 {cons- "std" cons}
 {testing "testing"}
 {fntest- "testing" fntestx}}

defn .switch {exp argz}
{if {call {/std/not- {call {/std/eq- {call {/std/head- argz}} 'brackets}}}}
 {return "expected brackets"}
 {}
 {return {list {'brackets 'args '= exp {call {/std/switch-fun {call {/std/tail- argz}}}}}}}}

.switch.arity = 3
defn .fntest {body expect}
{return {call {/testing/fntest- body expect}}}

.fntest.arity = 3
defn .backquote {exp}
{if {call {/std/not- {call {/std/eq- {call {/std/type- exp}} :list}}}}
 {return {list {'brackets 'quote exp}}}
 {if {call {/std/not- exp}}
  {return {list {'brackets 'quote exp}}}
  {if {call {/std/eq- {call {/std/length- exp}} 3}}
   {if {call {/std/eq- {call {/std/list-at- exp 1}} 'tilde}}
    {return {call {/std/list-at- exp 2}}}
    {return {list {'/std/cons {call {../.backquote {call {/std/head- exp}}}} {call {../.backquote {call {/std/tail- exp}}}}}}}}
   {return {list {'/std/cons {call {../.backquote {call {/std/head- exp}}}} {call {../.backquote {call {/std/tail- exp}}}}}}}}}}

.backquote.arity = 2
defn .defnx {name args body}
{return {list {'brackets 'defn name {list 'brackets} {list {'brackets 'defn '__magically_called__ args body {list 'brackets} '= {list {'__magically_called__ '@mut '@0 '@up2}} 'return :shouldnt-go-here}} {list 'brackets} '= {list {name '@mut '@0}}}}}

.defnx.arity = 4
