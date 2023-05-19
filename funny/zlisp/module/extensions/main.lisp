req
{(std "std")
 (switch-fun "std" switch-fun)
 (not- "std" not)
 (eq- "std" eq)
 (type- "std" type)
 (length- "std" length)
 (head- "std" head)
 (tail- "std" tail)
 (cons- "std" cons)
 (testing "testing")
 (fntest- "testing" fntestx)}

defn .switch (exp argz)
{if (/std/not- (/std/eq- (/std/head- argz) 'brackets))
 {return "expected brackets"}
 {}
 {return {list {'brackets 'args '= exp (/std/switch-fun (/std/tail- argz))}}}}

defn .fntest (body expect)
{return (/testing/fntest- body expect)}

defn .backquote (exp)
{if (/std/not- (/std/eq- (/std/type- exp) :list))
 {return {list {'brackets 'quote exp}}}
 {if (/std/not- exp)
  {return {list {'brackets 'quote exp}}}
  {if (/std/eq- (/std/length- exp) 2)
   {if (/std/eq- (/std/head- exp) 'tilde)
    {return (/std/head- (/std/tail- exp))}
    {return {list {'/std/cons (../.backquote (/std/head- exp)) (../.backquote (/std/tail- exp))}}}}
   {return {list {'/std/cons (../.backquote (/std/head- exp)) (../.backquote (/std/tail- exp))}}}}}}

defn .defn2 (name args body)
{return {list {'brackets 'defn name '() {list {'brackets 'defn '__magically_called__ args body '() '= {list {'__magically_called__ '@mut '@0 '@up2}} 'return :shouldnt-go-here}} '() '= {list {name '@mut '@0}}}}}
