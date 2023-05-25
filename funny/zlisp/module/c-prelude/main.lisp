req
[]

defn call-extension-1 {fnptr x}
[r = [return @1 @{host "call-extension"} [fnptr x]]
 return r]

deref-pointer = [return @1 @{host "deref-pointer"} [list []]]
defn deref {x y}
[r = [return @1 @{host "call-extension"} [deref-pointer x y]]
 return r]

mkptr-pointer = [return @1 @{host "mkptr-pointer"} [list []]]
defn mkptr {x y}
[r = [return @1 @{host "call-extension"} [mkptr-pointer x y]]
 return r]

pointer-call-pointer = [return @1 @{host "pointer-call-pointer"} [list []]]
defn pointer-call {x y z}
[r = [return @1 @{host "call-extension"} [pointer-call-pointer x y z]]
 return r]

panic-pointer = [return @1 @{host "panic"} [list []]]
defn panic {x}
[ignored-result = [return @1 @{host "call-extension"} [panic-pointer x]]
 return []]

head-pointer = [return @1 @{host "head"} [list []]]
defn head {x}
[r = [return @1 @{host "call-extension-1"} [head-pointer x]]
 return r]

tail-pointer = [return @1 @{host "tail"} [list []]]
defn tail {x}
[r = [return @1 @{host "call-extension-1"} [tail-pointer x]]
 return r]

cons-pointer = [return @1 @{host "cons"} [list []]]
defn cons {x xs}
[r = [return @1 @{host "call-extension"} [cons-pointer x xs]]
 return r]

eq-pointer = [return @1 @{host "eq"} [list []]]
defn eq {x y}
[r = [return @1 @{host "call-extension"} [eq-pointer x y]]
 return r]

defn serialize-param {param signature}
[if (../eq signature 'pointer)
 [return param]
 [if (../eq signature 'fdatum)
  [return param]
  [if (../eq signature 'progslice)
   [return param]
   [return (../mkptr param signature)]]]]

defn serialize-params {params signature}
[if params
 [return (../cons (../serialize-param (../head params) (../head signature)) (../serialize-params (../tail params) (../tail signature)))]
 [return [list []]]]

defn dereference {what how}
[if (../eq how 'pointer)
 [return what]
 [if (../eq how 'fdatum)
  [return what]
  [if (../eq how 'progslice)
   [return what]
   [return (../deref what how)]]]]

defn pointer-call-and-deserialize {fn-ptr signature params}
[fnparamst = (../head signature)
 rettype = (../head (../tail signature))
 s = (../serialize-params params fnparamst)
 rawres = (../pointer-call fn-ptr [list [fnparamst rettype]] s)
 return (../dereference rawres rettype)]

rtld-lazy = [return @1 @{host "RTLD_LAZY"} [list []]]
dlopen-pointer = [return @1 @{host "dlopen"} [list []]]
defn dlopen {x}
[return (../pointer-call-and-deserialize dlopen-pointer [list [[list ['string 'sizet]] 'pointer]] [list [x rtld-lazy]])]

defn dlopen-null {}
[return (../pointer-call-and-deserialize dlopen-pointer [list [[list ['pointer 'sizet]] 'pointer]] [list [(../mkptr 0 'sizet) rtld-lazy]])]

dlsym-pointer = [return @1 @{host "dlsym"} [list []]]
defn dlsym {x y}
[return (../pointer-call-and-deserialize dlsym-pointer [list [[list ['pointer 'string]] 'pointer]] [list [x y]])]

defn c-data-pointer {handle c-name signature}
[fn-pointer-pointer = (../dlsym handle c-name)
 fn-pointer = (../dereference fn-pointer-pointer 'int64)
 return fn-pointer]

defn nth {n xs}
[if xs
 [if n
  [return (../nth (../tail n) (../tail xs))]
  [return (../head xs)]]
 (../panic "nth fail")]

defn get-fn-ptr {handle c-name}
[fn-pointer-pointer = (../dlsym handle c-name)
 fn-ptr = (../dereference fn-pointer-pointer 'int64)
 if (../eq fn-ptr 0)
 (../panic "couldn't load C function")
 [return fn-ptr]]

defn c-function-0 {fn-ptr signature}
[return []
 return (../pointer-call-and-deserialize fn-ptr signature [list []])]

defn c-function-1 {fn-ptr signature}
[{a1} = [return @1 []]
 return (../pointer-call-and-deserialize fn-ptr signature [list a1])]

defn c-function-2 {fn-ptr signature}
[{a1 a2} = [return @2 []]
 return (../pointer-call-and-deserialize fn-ptr signature [list [a1 a2]])]

defn c-function-3 {fn-ptr signature}
[{a1 a2 a3} = [return @3 []]
 return (../pointer-call-and-deserialize fn-ptr signature [list [a1 a2 a3]])]

defn c-function-4 {fn-ptr signature}
[{a1 a2 a3 a4} = [return @4 []]
 return (../pointer-call-and-deserialize fn-ptr signature [list [a1 a2 a3 a4]])]

defn c-function-5 {fn-ptr signature}
[{a1 a2 a3 a4 a5} = [return @5 []]
 return (../pointer-call-and-deserialize fn-ptr signature [list [a1 a2 a3 a4 a5]])]

defn c-function-6 {fn-ptr signature}
[{a1 a2 a3 a4 a5 a6} = [return @6 []]
 return (../pointer-call-and-deserialize fn-ptr signature [list [a1 a2 a3 a4 a5 a6]])]

defn c-function-7 {fn-ptr signature}
[{a1 a2 a3 a4 a5 a6 a7} = [return @7 []]
 return (../pointer-call-and-deserialize fn-ptr signature [list [a1 a2 a3 a4 a5 a6 a7]])]

defn c-function-8 {fn-ptr signature}
[{a1 a2 a3 a4 a5 a6 a7 a8} = [return @8 []]
 return (../pointer-call-and-deserialize fn-ptr signature [list [a1 a2 a3 a4 a5 a6 a7 a8]])]

defn c-function {handle c-name signature}
[argssig = (../head signature)
 objs = [list [c-function-0 c-function-1 c-function-2 c-function-3 c-function-4 c-function-5 c-function-6 c-function-7 c-function-8]]
 obj = (../nth argssig objs)
 fn-ptr = (../get-fn-ptr handle c-name)
 {} = (../obj @0 @mut fn-ptr signature)
 return obj]

selflib = (dlopen-null)
annotate-pointer = (dereference (dlsym selflib "builtin_annotate") 'int64)
defn annotate {x}
[r = [return @1 @{host "call-extension"} [annotate-pointer x]]
 return r]

is-constant-pointer = (dereference (dlsym selflib "builtin_is_constant") 'int64)
defn is-constant {x}
[r = [return @1 @{host "call-extension"} [is-constant-pointer x]]
 return r]

repr-pointer = (dereference (dlsym selflib "builtin_repr") 'int64)
defn repr {x}
[r = [return @1 @{host "call-extension"} [repr-pointer x]]
 return r]

concat-bytestrings-pointer = (dereference (dlsym selflib "builtin_concat_bytestrings") 'int64)
defn concat-bytestrings {x y}
[r = [return @1 @{host "call-extension"} [concat-bytestrings-pointer x y]]
 return r]

+-pointer = (dereference (dlsym selflib "builtin_add") 'int64)
defn + {x y}
[r = [return @1 @{host "call-extension"} [+-pointer x y]]
 return r]

defn wrap-pointer-into-pointer {p}
[return (../mkptr p 'sizet)]

defn shared-library {path}
[r = (../dlopen path)
 if (../eq 0 (../dereference r 'int64))
 [return [list [:err "shared-library failed"]]]
 [return [list [:ok r]]]]

defn extern-pointer {handle c-name signature}
[res = (../c-data-pointer handle c-name signature)
 if (../eq 0 res)
 [return [list [:err "extern-pointer failed"]]]
 [return [list [:ok res]]]]

export
[[call-extension-1 call-extension-1]
 [panic panic]
 [head head]
 [tail tail]
 [cons cons]
 [eq eq]
 [dereference dereference]
 [dlsym dlsym]
 [c-function c-function]
 [eq eq]
 [annotate annotate]
 [is-constant is-constant]
 [repr repr]
 [concat-bytestrings concat-bytestrings]
 [+ +]
 [wrap-pointer-into-pointer wrap-pointer-into-pointer]
 [shared-library shared-library]
 [extern-pointer extern-pointer]
 [selflib selflib]]
