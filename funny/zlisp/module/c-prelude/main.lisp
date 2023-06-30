req
{}

deref-pointer := return @1

@{host
 "deref-pointer"}

{}

deref := fn {x y}
{r := return @1
 @{host
  "call-extension"}
 ^{deref-pointer
  x
  y}
 return r}

mkptr-pointer := return @1

@{host
 "mkptr-pointer"}

{}

mkptr := fn {x y}
{r := return @1
 @{host
  "call-extension"}
 ^{mkptr-pointer
  x
  y}
 return r}

pointer-call-pointer := return @1

@{host
 "pointer-call-pointer"}

{}

pointer-call := fn {x y z}
{r := return @1
 @{host
  "call-extension"}
 ^{pointer-call-pointer
  x
  y
  z}
 return r}

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
  "call-extension"}
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
  "call-extension"}
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
  "call-extension"}
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
  "call-extension"}
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

serialize-param := fn {param signature}
{if (../eq signature 'pointer)
 {return param}
 {if (../eq signature 'progslice)
  {return param}
  {return (../mkptr param signature)}}}

serialize-params := 42

serialize-params = fn {params signature}
{if params
 {return (../cons (../serialize-param (../head params) (../head signature)) (../serialize-params (../tail params) (../tail signature)))}
 {return {}}}

dereference := fn {what how}
{if (../eq how 'pointer)
 {return what}
 {if (../eq how 'progslice)
  {return what}
  {return (../deref what how)}}}

pointer-call-and-deserialize := fn {fn-ptr signature params}
{fnparamst := (../head signature)
 rettype := (../head (../tail signature))
 s := (../serialize-params params fnparamst)
 rawres := (../pointer-call fn-ptr {fnparamst
   rettype} s)
 return (../dereference rawres rettype)}

rtld-lazy := return @1

@{host
 "RTLD_LAZY"}

{}

dlopen-pointer := return @1

@{host
 "dlopen"}

{}

dlopen := fn {x}
{return (../pointer-call-and-deserialize dlopen-pointer {{'string
    'sizet}
   'int64} {x
   rtld-lazy})}

dlopen-or-error := fn {path}
{r := (../dlopen path)
 if (../eq 0 r)
 {return {:err
   "dlopen-or-error failed"}}
 {return {:ok
   r}}}

dlopen-null := fn {}
{return (../pointer-call-and-deserialize dlopen-pointer {{'pointer
    'sizet}
   'int64} {(../mkptr 0 'sizet)
   rtld-lazy})}

dlsym-pointer := return @1

@{host
 "dlsym"}

{}

dlsym := fn {x y}
{return (../pointer-call-and-deserialize dlsym-pointer {{'int64
    'string}
   'pointer} {x
   y})}

dlsym-or-error := fn {handle c-name}
{res := (../dlsym handle c-name)
 resval := (../deref res 'int64)
 if (../eq 0 resval)
 {return {:err
   "dlsym-or-error failed"}}
 {return {:ok
   resval}}}


dlsym-or-panic := fn {handle c-name}
{
 res := (../dlsym-or-error handle c-name)
 if (../eq (../head res) :err) {
 {} := (../panic @0 "couldn't load C function")
 } {
  return (../head (../tail res))
 }
}

c-function-0 := fn {fn-ptr signature}
{return ^{}
 return (../pointer-call-and-deserialize fn-ptr signature {})}

c-function-1 := fn {fn-ptr signature}
{a1 := return @1
 ^{}
 return (../pointer-call-and-deserialize fn-ptr signature {a1})}

c-function-2 := fn {fn-ptr signature}
{{a1 a2} := return @2
 ^{}
 return (../pointer-call-and-deserialize fn-ptr signature {a1
   a2})}

c-function-3 := fn {fn-ptr signature}
{{a1 a2 a3} := return @3
 ^{}
 return (../pointer-call-and-deserialize fn-ptr signature {a1
   a2
   a3})}

c-function-4 := fn {fn-ptr signature}
{{a1 a2 a3 a4} := return @4
 ^{}
 return (../pointer-call-and-deserialize fn-ptr signature {a1
   a2
   a3
   a4})}

c-function-5 := fn {fn-ptr signature}
{{a1 a2 a3 a4 a5} := return @5
 ^{}
 return (../pointer-call-and-deserialize fn-ptr signature {a1
   a2
   a3
   a4
   a5})}

c-function-6 := fn {fn-ptr signature}
{{a1 a2 a3 a4 a5 a6} := return @6
 ^{}
 return (../pointer-call-and-deserialize fn-ptr signature {a1
   a2
   a3
   a4
   a5
   a6})}

c-function-7 := fn {fn-ptr signature}
{{a1 a2 a3 a4 a5 a6 a7} := return @7
 ^{}
 return (../pointer-call-and-deserialize fn-ptr signature {a1
   a2
   a3
   a4
   a5
   a6
   a7})}

c-function-8 := fn {fn-ptr signature}
{{a1 a2 a3 a4 a5 a6 a7 a8} := return @8
 ^{}
 return (../pointer-call-and-deserialize fn-ptr signature {a1
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

selflib := (dlopen-null)

annotate-pointer := (dlsym-or-panic selflib "builtin_annotate")

annotate := fn {x}
{r := return @1
 @{host
  "call-extension"}
 ^{annotate-pointer 
  x}
 return r}

is-constant-pointer := (dlsym-or-panic selflib "builtin_is_constant")

is-constant := fn {x}
{r := return @1
 @{host
  "call-extension"}
 ^{is-constant-pointer 
  x}
 return r}

repr-pointer := (dlsym-or-panic selflib "builtin_repr")

repr := fn {x}
{r := return @1
 @{host
  "call-extension"}
 ^{repr-pointer 
  x}
 return r}

concat-bytestrings-pointer := (dlsym-or-panic selflib "builtin_concat_bytestrings")

concat-bytestrings := fn {x y}
{r := return @1
 @{host
  "call-extension"}
 ^{concat-bytestrings-pointer 
  x
  y}
 return r}

+-pointer := (dlsym-or-panic selflib "builtin_add")

+ := fn {x y}
{r := return @1
 @{host
  "call-extension"}
 ^{+-pointer 
  x
  y}
 return r}

wrap-pointer-into-pointer := fn {p}
{return (../mkptr p 'sizet)}

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
 {c-function c-function}
 {wrap-pointer-into-pointer wrap-pointer-into-pointer}
 {selflib selflib}}
