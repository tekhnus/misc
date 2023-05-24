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

defn panic {x}
{{call
  {/prelude/panic-
   @0
   x}}
 {return {}}}

defn head {x}
{return {call {/prelude/head- x}}}

defn tail {x}
{return {call {/prelude/tail- x}}}

defn cons {x xs}
{return {call {/prelude/cons- x xs}}}

defn eq {x y}
{return {call {/prelude/eq- x y}}}

defn annotate {x}
{return {call {/prelude/annotate- x}}}

defn is-constant {x}
{return {call {/prelude/is-constant- x}}}

defn repr {x}
{return {call {/prelude/repr- x}}}

defn concat-bytestrings {x y}
{return {call {/prelude/concat-bytestrings- x y}}}

defn + {x y}
{return {call {/prelude/+- x y}}}

defn not {x}
{if x
 {return {list {}}}
 {return {list {{list {}}}}}}

defn last {a0}
{if {call {../tail a0}}
 {return {call {../last {call {../tail a0}}}}}
 {return {call {../head a0}}}}

defn type {x}
{return {call {../head {call {../annotate x}}}}}

defn length {x}
{n = 0
 while x
 {n = {call {../+ n 1}}
  x = {call {../tail x}}}
 {return n}}

defn concat {a0 a1}
{if a0
 {return {call {../cons {call {../head a0}} {call {../concat {call {../tail a0}} a1}}}}}
 {return a1}}

defn zip {a0 a1}
{if a0
 {return {call {../cons {list {{call {../head a0}} {call {../head a1}}}} {call {../zip {call {../tail a0}} {call {../tail a1}}}}}}}
 {return {list {}}}}

defn map {a0 a1}
{if a1
 {return {call {../cons {call {../a0 {call {../head a1}}}} {call {../map a0 {call {../tail a1}}}}}}}
 {return {list {}}}}

panic-block = {list {'argz {list {'/std/panic '@0 '"wrong fn call"}}}}
defn list-at {xs n}
{if {call {../eq n 0}}
 {{return {call {../head xs}}}}
 {{return {call {../list-at {call {../tail xs}} {call {../+ n -1}}}}}}}

defn swtchone {a0}
{if a0
 {firstarg = {call {../head a0}}
  cond = {call {../head firstarg}}
  body = {call {../list-at firstarg 1}}
  rest = {call {../swtchone {call {../tail a0}}}}
  {return {list {'brackets 'prearg '= cond 'if {list {'/std/eq {list {'/std/head 'prearg}} ':ok}} {list {'brackets 'args '= {list {'/std/list-at 'prearg '1}} body}} rest}}}}
 {return {list {'/std/panic '@0 '"nothing matched"}}}}

defn decons-pat {a0 a1}
{pat = a0
 val = a1
 if {call {../is-constant pat}}
 {{if {call {../eq pat val}}
   {return {list {':ok {list {}}}}}
   {return {list {':err}}}}}
 {if {call {../eq {call {../type pat}} :symbol}}
  {{return {list {:ok {list val}}}}}
  {if {call {../eq {call {../type pat}} :list}}
   {if pat
    {if val
     {first-decons = {call {../decons-pat {call {../head pat}} {call {../head val}}}}
      rest-decons = {call {../decons-pat {call {../tail pat}} {call {../tail val}}}}
      {if {call {../eq :err {call {../head rest-decons}}}}
       {return {list {':err}}}
       {if {call {../eq :err {call {../head first-decons}}}}
        {return {list {':err}}}
        {return {list {:ok {call {../concat {call {../list-at first-decons 1}} {call {../list-at rest-decons 1}}}}}}}}}}
     {{return {list {':err}}}}}
    {if val
     {{return {list {':err}}}}
     {{return {list {':ok {list {}}}}}}}}
   {{call
     {../panic
      @0
      "decons-pat met an unsupported type"}}}}}}

defn decons-vars {a0}
{if {call {../is-constant a0}}
 {return {list {}}}
 {if {call {../eq {call {../type a0}} :symbol}}
  {return {list {a0}}}
  {if {call {../eq {call {../type a0}} :list}}
   {if a0
    {return {call {../concat {call {../decons-vars {call {../head a0}}}} {call {../decons-vars {call {../tail a0}}}}}}}
    {return {list {}}}}
   {call
    {panic
     @0
     "decons-var met an unsupported type"}}}}}

switch-defines = {list {{list {'/std/list-at 'args '0}} {list {'/std/list-at 'args '1}} {list {'/std/list-at 'args '2}} {list {'/std/list-at 'args '3}} {list {'/std/list-at 'args '4}} {list {'/std/list-at 'args '5}}}}
defn make-assignment {x}
{return {list {'brackets {call {../head x}} '= {call {../list-at x 1}}}}}

defn switch-clause {a0}
{if {call {../not {call {../eq {call {../head a0}} 'brackets}}}}
 {return "expected brackets"}
 {}
 a1 = {call {../tail a0}}
 sig = {call {../head a1}}
 warning = "brackets"
 {if (../eq (../type sig) :list) {sig = (../tail sig)} {}}
 cmds = {call {../tail a1}}
 checker = {list {'/std/decons-pat {list {'brackets 'quote sig}} 'args}}
 vars = {call {../decons-vars sig}}
 body = {call {../cons 'brackets {call {../concat {call {../map make-assignment {call {../zip vars switch-defines}}}} cmds}}}}
 {return {list {checker body}}}}

defn switch-fun {a0}
{return {call {../swtchone {call {../map switch-clause a0}}}}}

defn append {x xs}
{if xs
 {return {call {../cons {call {../head xs}} {call {../append x {call {../tail xs}}}}}}}
 {return {list {x}}}}

defn first-good-value {x}
{if x
 {first-arg = {call {../head x}}
  {if {call {../eq :ok {call {../head first-arg}}}}
   {{return {call {../list-at first-arg 1}}}}
   {{return {call {../first-good-value {call {../tail x}}}}}}}}
 {call
  {panic
   @0
   "first-good-value: no good value"}}}

export
{{panic panic}
 {head head}
 {tail tail}
 {cons cons}
 {eq eq}
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
