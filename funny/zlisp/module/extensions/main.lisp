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
{if (/std/not- (/std/eq- (/std/head- argz) {quote {brackets}}))
 {return "expected brackets"}
 {}
 {return {list {{quote {brackets}} {quote {args}} {quote {=}} exp (/std/switch-fun (/std/tail- argz))}}}}

.switch.arity = 3
defn .fntest {body expect}
{return (/testing/fntest- body expect)}

.fntest.arity = 3
defn .backquote {exp}
{if (/std/not- (/std/eq- (/std/type- exp) :list))
 {return {list {{quote {brackets}} {quote {quote}} {list {{quote {brackets}} exp}}}}}
 {if (/std/not- exp)
  {return {list {{quote {brackets}} {quote {quote}} {list {{quote {brackets}} exp}}}}}
  {if (/std/eq- (/std/length- exp) 3)
   {if (/std/eq- (/std/list-at- exp 1) {quote {tilde}})
    {return (/std/list-at- exp 2)}
    {return {list {{quote {/std/cons}} (../.backquote (/std/head- exp)) (../.backquote (/std/tail- exp))}}}}
   {return {list {{quote {/std/cons}} (../.backquote (/std/head- exp)) (../.backquote (/std/tail- exp))}}}}}}

.backquote.arity = 2
defn .defnx {name args body}
{return {list {{quote {brackets}} {quote {defn}} name {list {quote {brackets}}} {list {{quote {brackets}} {quote {defn}} {quote {__magically_called__}} args body {list {quote {brackets}}} {quote {=}} {list {{quote {__magically_called__}} {quote {@mut}} {quote {@0}} {quote {@up2}}}} {quote {return}} :shouldnt-go-here}} {list {quote {brackets}}} {quote {=}} {list {name {quote {@mut}} {quote {@0}}}}}}}

.defnx.arity = 4
