req
{{std "std"} {eq "std" eq} {decons-pat "std" decons-pat} {head "std" head} {concat-bytestrings "std" concat-bytestrings} {panic "std" panic}}

defn fntestx {body expect}
{repr-expect = {list {'call {list {'/std/repr expect}}}} fact-equals-expect = {list {'call {list {'/std/eq 'val expect}}}} new-panic = {list {'call {list {'/std/concat-bytestrings '(/std/concat-bytestrings (/std/repr val) " != ") repr-expect}}}} append-panics = {list {'panics '= {list {'call {list {'/std/cons new-panic 'panics}}}}}} return {list {'defn 'calltest '{} body 'val '= '(calltest) 'if fact-equals-expect {list {'panics '= 'panics}} append-panics}}}

export
{{fntestx fntestx}}
