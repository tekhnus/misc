req
{{std "std"}
 {eq "std" eq}
 {decons-pat "std" decons-pat}
 {head "std" head}
 {concat-bytestrings "std" concat-bytestrings}
 {panic "std" panic}}

defn fntestx {body expect}
{return {list {'brackets 'defn 'calltest {list 'brackets} body 'val '= {list {'calltest}} 'if {list {'/std/eq 'val expect}} {list {'brackets 'panics '= 'panics}} {list {'brackets 'panics '= {list {'/std/cons {list {'/std/concat-bytestrings {list {'/std/concat-bytestrings {list {'/std/repr 'val}} '" != "}} {list {'/std/repr expect}}}} 'panics}}}}}}}

export
{{fntestx fntestx}}
