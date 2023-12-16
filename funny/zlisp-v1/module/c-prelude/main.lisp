req
{}

call-ffi-pointer := return @1

@{host
 "call-ffi"}

{}

call-ffi := fn {x y z}
{r := return @1
 @{host
  "call-builtin"}
 ^{call-ffi-pointer
  x
  y
  z}
 return r}

len-pointer := return @1

@{host
 "len"}

{}

len := fn {x}
{r := return @1
 @{host
  "call-builtin"}
 ^{len-pointer
  x}
 return r}

deref-pointer := return @1

@{host
 "deref"}

{}

deref := fn {x y}
{r := return @1
 @{host
  "call-builtin"}
 ^{deref-pointer
  x
  y}
 return r}

serialize-pointer := return @1

@{host
 "serialize"}

{}

ser := fn {x}
{r := return @1
 @{host
  "call-builtin"}
 ^{serialize-pointer
  x}
 return r}

malloc-pointer := return @1

@{host
 "malloc"}

{}

malloc := fn {n}
{return (../call-ffi malloc-pointer {{'sizet}
   'pointer} {n})}

copy-to-memory-pointer := return @1

@{host
 "copy-to-memory"}

{}

malloc-and-copy := fn {x}
{sz := (../len x)
 sz-ser := (../ser sz)
 mem := (../malloc sz-ser)
 {} := return @0
 @{host
  "call-builtin"}
 ^{copy-to-memory-pointer
  mem
  x}
 return mem}

panic := fn {x}
{{} := return @panic
 x
 return ^{}}

head-pointer := return @1

@{host
 "head"}

{}

head := fn {x}
{r := return @1
 @{host
  "call-builtin"}
 ^{head-pointer
  x}
 return r}

tail-pointer := return @1

@{host
 "tail"}

{}

tail := fn {x}
{r := return @1
 @{host
  "call-builtin"}
 ^{tail-pointer
  x}
 return r}

cons-pointer := return @1

@{host
 "cons"}

{}

cons := fn {x xs}
{r := return @1
 @{host
  "call-builtin"}
 ^{cons-pointer
  x
  xs}
 return r}

eq-pointer := return @1

@{host
 "eq"}

{}

eq := fn {x y}
{r := return @1
 @{host
  "call-builtin"}
 ^{eq-pointer
  x
  y}
 return r}

nth := 42

nth = fn {n xs}
{if xs
 {if n
  {return (../nth (../tail n) (../tail xs))}
  {return (../head xs)}}
 {{} := (../panic @0 "nth fail")}}

null-pointer := return @1

@{host
 "null"}

{}

rtld-lazy := return @1

@{host
 "RTLD_LAZY"}

{}

dlopen-pointer := return @1

@{host
 "dlopen"}

{}

dlopen := fn {x}
{x-ser := (../ser x)
 x-on-heap := (../malloc-and-copy x-ser)
 if (../eq x "__magic_null_string__")
 {x-on-heap = null-pointer}
 {}
 return (../call-ffi dlopen-pointer {{'pointer
    'int}
   'pointer} {x-on-heap
   rtld-lazy})}

dlopen-or-error := fn {path}
{r := (../dlopen path)
 if (../eq null-pointer r)
 {return {:err
   "dlopen-or-error failed"}}
 {return {:ok
   r}}}

dlsym-pointer := return @1

@{host
 "dlsym"}

{}

dlsym := fn {x y}
{y-ser := (../ser y)
 y-on-heap := (../malloc-and-copy y-ser)
 return (../call-ffi dlsym-pointer {{'pointer
    'pointer}
   'pointer} {x
   y-on-heap})}

dlsym-or-error := fn {handle c-name}
{resval := (../dlsym handle c-name)
 if (../eq null-pointer resval)
 {return {:err
   "dlsym-or-error failed"}}
 {return {:ok
   resval}}}

dlsym-or-panic := fn {handle c-name}
{res := (../dlsym-or-error handle c-name)
 if (../eq (../head res) :err)
 {{} := (../panic @0 "couldn't load C function")}
 {return (../head (../tail res))}}

c-function-0 := fn {fn-ptr signature}
{return ^{}
 return (../call-ffi fn-ptr signature {})}

c-function-1 := fn {fn-ptr signature}
{a1 := return @1
 ^{}
 return (../call-ffi fn-ptr signature {a1})}

c-function-2 := fn {fn-ptr signature}
{{a1 a2} := return @2
 ^{}
 return (../call-ffi fn-ptr signature {a1
   a2})}

c-function-3 := fn {fn-ptr signature}
{{a1 a2 a3} := return @3
 ^{}
 return (../call-ffi fn-ptr signature {a1
   a2
   a3})}

c-function-4 := fn {fn-ptr signature}
{{a1 a2 a3 a4} := return @4
 ^{}
 return (../call-ffi fn-ptr signature {a1
   a2
   a3
   a4})}

c-function-5 := fn {fn-ptr signature}
{{a1 a2 a3 a4 a5} := return @5
 ^{}
 return (../call-ffi fn-ptr signature {a1
   a2
   a3
   a4
   a5})}

c-function-6 := fn {fn-ptr signature}
{{a1 a2 a3 a4 a5 a6} := return @6
 ^{}
 return (../call-ffi fn-ptr signature {a1
   a2
   a3
   a4
   a5
   a6})}

c-function-7 := fn {fn-ptr signature}
{{a1 a2 a3 a4 a5 a6 a7} := return @7
 ^{}
 return (../call-ffi fn-ptr signature {a1
   a2
   a3
   a4
   a5
   a6
   a7})}

c-function-8 := fn {fn-ptr signature}
{{a1 a2 a3 a4 a5 a6 a7 a8} := return @8
 ^{}
 return (../call-ffi fn-ptr signature {a1
   a2
   a3
   a4
   a5
   a6
   a7
   a8})}

c-function := fn {handle c-name signature}
{argssig := (../head signature)
 objs := {c-function-0
  c-function-1
  c-function-2
  c-function-3
  c-function-4
  c-function-5
  c-function-6
  c-function-7
  c-function-8}
 obj := (../nth argssig objs)
 fn-ptr-val := (../dlsym-or-panic handle c-name)
 {} := (../obj @0 @mut fn-ptr-val signature)
 return obj}

selflib := (dlopen "__magic_null_string__")

annotate-pointer := (dlsym-or-panic selflib "builtin_annotate")

annotate := fn {x}
{r := return @1
 @{host
  "call-builtin"}
 ^{annotate-pointer
  x}
 return r}

is-constant-pointer := (dlsym-or-panic selflib "builtin_is_constant")

is-constant := fn {x}
{r := return @1
 @{host
  "call-builtin"}
 ^{is-constant-pointer
  x}
 return r}

repr-pointer := (dlsym-or-panic selflib "builtin_repr")

repr := fn {x}
{r := return @1
 @{host
  "call-builtin"}
 ^{repr-pointer
  x}
 return r}

concat-bytestrings-pointer := (dlsym-or-panic selflib "builtin_concat_bytestrings")

concat-bytestrings := fn {x y}
{r := return @1
 @{host
  "call-builtin"}
 ^{concat-bytestrings-pointer
  x
  y}
 return r}

+-pointer := (dlsym-or-panic selflib "builtin_add")

+ := fn {x y}
{r := return @1
 @{host
  "call-builtin"}
 ^{+-pointer
  x
  y}
 return r}

export
{{head head}
 {tail tail}
 {cons cons}
 {eq eq}
 {annotate annotate}
 {is-constant is-constant}
 {repr repr}
 {concat-bytestrings concat-bytestrings}
 {+ +}
 {dlopen-or-error dlopen-or-error}
 {dlsym-or-error dlsym-or-error}
 {null-pointer null-pointer}
 {c-function c-function}
 {deref deref}
 {ser ser}
 {malloc-and-copy malloc-and-copy}
 {selflib selflib}}
