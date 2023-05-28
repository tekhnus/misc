req
{}

print := fn {a0}

{r := {return @1
  @{host
   "eval"
   "print(x[1][0])"}
  {list
   a0}}
 return r}

debug-print := fn {a0}

{r := {return @1
  @{host
   "eval"
   "print(x[1][0])"}
  {list
   a0}}
 return r}

head := fn {a0}

{r := {return @1
  @{host
   "eval"
   "head(x)"}
  {list
   a0}}
 return r}

tail := fn {a0}

{r := {return @1
  @{host
   "eval"
   "tail(x)"}
  {list
   a0}}
 return r}

is-constant := fn {a0}

{r := {return @1
  @{host
   "eval"
   "is_constant(x)"}
  {list
   a0}}
 return r}

eq := fn {a0 a1}

{r := {return @1
  @{host
   "eval"
   "eq(x)"}
  {list
   {a0
    a1}}}
 return r}

panic := fn {a0}

{ignored-result := {return @1
  @{host
   "eval"
   "panic(x)"}
  {list
   a0}}
 return {}}

annotate := fn {a0}

{r := {return @1
  @{host
   "eval"
   "annotate(x)"}
  {list
   a0}}
 return r}

+ := fn {a0 a1}

{r := {return @1
  @{host
   "eval"
   "add(x)"}
  {list
   {a0
    a1}}}
 return r}

cons := fn {a0 a1}

{r := {return @1
  @{host
   "eval"
   "cons(x)"}
  {list
   {a0
    a1}}}
 return r}

concat-bytestrings := fn {a0 a1}

{r := {return @1
  @{host
   "eval"
   "concat_bytestrings(x)"}
  {list
   {a0
    a1}}}
 return r}

repr := fn {a0}

{r := {return @1
  @{host
   "eval"
   "repre(x)"}
  {list
   a0}}
 return r}

export
{{print print}
 {panic panic}
 {head head}
 {tail tail}
 {cons cons}
 {eq eq}
 {annotate annotate}
 {is-constant is-constant}
 {repr repr}
 {concat-bytestrings concat-bytestrings}
 {+ +}}
