req
{{std "std"}
 {eq "std" eq}
 {decons-pat "std" decons-pat}
 {head "std" head}
 {concat-bytestrings "std" concat-bytestrings}
 {panic "std" panic}}

fntestx := fn {body expect}
{repr-expect := #{'call
  #{'/std/repr
   expect}}
 fact-equals-expect := #{'call
  #{'/std/eq
   'val
   expect}}
 new-panic := #{'call
  #{'/std/concat-bytestrings
   '(/std/concat-bytestrings
    (/std/repr val)
    " != ")
   repr-expect}}
 append-panics := #{'panics
  '=
  #{'call
   #{'/std/cons
    new-panic
    'panics}}}
 return #{'calltest
  '=
  #{'fn
   '{}
   body}
  'val
  '=
  '(calltest)
  'if
  fact-equals-expect
  #{'panics
   '=
   'panics}
  append-panics}}

export
{{fntestx fntestx}}
