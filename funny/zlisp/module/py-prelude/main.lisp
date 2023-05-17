req
{}

defn print (a0)
{r = {return @1 @(host ("eval" "print(x[1][0])")) {list a0}}
 return r}

defn debug-print (a0)
{r = {return @1 @(host ("eval" "print(x[1][0])")) {list a0}}
 return r}

defn head (a0)
{r = {return @1 @(host ("eval" "head(x)")) {list a0}}
 return r}

defn tail (a0)
{r = {return @1 @(host ("eval" "tail(x)")) {list a0}}
 return r}

defn is-constant (a0)
{r = {return @1 @(host ("eval" "is_constant(x)")) {list a0}}
 return r}

defn eq (a0 a1)
{r = {return @1 @(host ("eval" "eq(x)")) {list {a0 a1}}}
 return r}

defn panic (a0)
{ignored-result = {return @1 @(host ("eval" "panic(x)")) {list a0}}
 return {}}

defn annotate (a0)
{r = {return @1 @(host ("eval" "annotate(x)")) {list a0}}
 return r}

defn + (a0 a1)
{r = {return @1 @(host ("eval" "add(x)")) {list {a0 a1}}}
 return r}

defn cons (a0 a1)
{r = {return @1 @(host ("eval" "cons(x)")) {list {a0 a1}}}
 return r}

defn concat-bytestrings (a0 a1)
{r = {return @1 @(host ("eval" "concat_bytestrings(x)")) {list {a0 a1}}}
 return r}

defn repr (a0)
{r = {return @1 @(host ("eval" "repre(x)")) {list a0}}
 return r}

export
{(print print)
 (panic panic)
 (head head)
 (tail tail)
 (cons cons)
 (eq eq)
 (eq eq)
 (annotate annotate)
 (is-constant is-constant)
 (repr repr)
 (concat-bytestrings concat-bytestrings)
 (+ +)}
