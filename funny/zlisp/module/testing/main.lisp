req
[[std "std"]
 [eq "std" eq]
 [decons-pat "std" decons-pat]
 [head "std" head]
 [concat-bytestrings "std" concat-bytestrings]
 [panic "std" panic]]

defn fntestx {body expect}
[repr-expect = [list ['brackets 'call [list ['/std/repr expect]]]]
 fact-equals-expect = [list ['brackets 'call [list ['/std/eq 'val expect]]]]
 new-panic = [list ['brackets 'call [list ['/std/concat-bytestrings '(/std/concat-bytestrings (/std/repr val) " != ") repr-expect]]]]
 append-panics = [list ['brackets 'panics '= [list ['brackets 'call [list ['/std/cons new-panic 'panics]]]]]]
 return [list ['brackets 'defn 'calltest '{} body 'val '= '(calltest) 'if fact-equals-expect [list ['brackets 'panics '= 'panics]] append-panics]]]

export
[[fntestx fntestx]]
