req
{{std "std"}
 {eq "std" eq}
 {decons-pat "std" decons-pat}
 {head "std" head}
 {concat-bytestrings "std" concat-bytestrings}
 {panic "std" panic}}

defn fntestx {body expect}
{return {list {{quote {brackets}} {quote {defn}} {quote {calltest}} {list {quote {brackets}}} body {quote {val}} {quote {=}} {list {quote {calltest}}} {quote {if}} {list {{quote {/std/eq}} {quote {val}} expect}} {list {{quote {brackets}} {quote {panics}} {quote {=}} {quote {panics}}}} {list {{quote {brackets}} {quote {panics}} {quote {=}} {list {{quote {/std/cons}} {list {{quote {/std/concat-bytestrings}} {list {quote {/std/concat-bytestrings (/std/repr val) " != "}}} {list {{quote {/std/repr}} expect}}}} {quote {panics}}}}}}}}}

export
{{fntestx fntestx}}
