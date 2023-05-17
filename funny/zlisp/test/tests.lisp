req
{(prelude "prelude")
 (fprintf "libc" fprintf)
 (stderr "libc" stderr)
 (std "std")
 (decons-pat "std" decons-pat)
 (eq "std" eq)
 (head "std" head)
 (repr "std" repr)
 (append "std" append)
 (length "std" length)
 (list-at "std" list-at)
 (type "std" type)
 (+ "std" +)
 (panic "std" panic)
 (concat-bytestrings "std" concat-bytestrings)
 (cons "std" cons)
 (tail "std" tail)
 (panic "std" panic)
 (not "std" not)
 (libc "libc")
 (print "libc" print)}

panics = '()
fntest {return "hello, world!"} "hello, world!"
fntest {return (/std/+ 4 3)} 7
fntest {return (/std/length '(1 2 3))} 3
fntest {return (/std/length '())} 0
fntest {return (/std/type '())} :list
fntest {return `42} 42
fntest {return `(42 33)} '(42 33)
fntest {return `(42 33 55)} '(42 33 55)
fntest {return `()} '()
fntest {return `(42 (33 1))} '(42 (33 1))
fntest {return `(42 (/std/+ 33 1))} '(42 (/std/+ 33 1))
fntest {return `(42 ~(/std/+ 33 1))} '(42 34)
fntest {return `(42 ~{list {(/std/+ 33 1) `foo}})} '(42 (34 foo))
fntest {return {list {(/std/+ 4 3) 8}}} '(7 8)
fntest {return (/std/list-at '(1 2) 1)} 2
fntest {return (/std/eq :foo :bar)} '()
fntest {bar = :foo {return (/std/eq :foo bar)}} '(())
fntest {return (/std/append 5 '(1 2 3 4))} '(1 2 3 4 5)
fntest {return `(1 2 ~(/std/+ 1 2))} '(1 2 3)
fntest {defn twice (arg) {return (/std/+ arg arg)} {return (twice 35)}} 70
fntest {defn2 twice (arg) {return (/std/+ arg arg)} {return (twice 35)}} 70
fntest {defn2 twice (arg) {return (/std/+ arg arg)} defn four-times (arg) {return (/std/+ (../twice arg) (../twice arg))} {return (four-times 35)}} 140
fntest {defn fib (n) {if (/std/eq n 0) {return 0} {if (/std/eq n 1) {return 1} {return (/std/+ (../fib (/std/+ n -1)) (../fib (/std/+ n -2)))}}} return (fib 5)} 5
fntest {x = 0 y = 1 while (/std/not (/std/eq x 5)) {y = (/std/+ y y) x = (/std/+ x 1)} {return y}} 32
fntest {defn adderf (n) {m = {return @1 {}} {return (/std/+ n m)}} defn adder (n) {a = adderf () = (../a @0 @mut n) {return a}} {return ((adder 3) 4)}} 7
fntest {defn fib () {{return 3} {return 5} {return 8} {return 13}} (x) = (fib @mut) (y) = (fib @mut) (z) = (fib @mut) (t) = (fib @mut) {return `(~x ~y ~z ~t)}} '(3 5 8 13)
fntest {defn far-fib () {{return @event-loop 3} {return @event-loop 5} {return 8}} defn more-far-fib () {x = (../far-fib) {return @event-loop x} {return @event-loop 13}} (x) = (more-far-fib @mut @event-loop) (y) = (more-far-fib @mut @event-loop) (z) = (more-far-fib @mut @event-loop) (t) = (more-far-fib @mut @event-loop) {return `(~x ~y ~z ~t)}} '(3 5 8 13)
fntest {res = (/prelude/fprintf stderr "hello") {return 42}} 42
fntest {defn multi-ret () {() = {return {42 34}}} (x y) = (multi-ret @2) {return `(~x ~y)}} '(42 34)
fntest {defn foo (x) {y = {return @1 (/std/+ x 1)} (z t) = {return @2 (/std/+ y 1)} {return :done}} fee = foo a = (fee @mut 41) b = (fee @mut 33) c = (fee @mut 14 15) {return `(~a ~b ~c)}} '(42 34 :done)
fntest {defn cl-holder (x xs) {() = {return {}} () = {return {x xs}}} defn cl-cons (x xs) {holder = cl-holder () = (../holder @0 @mut x xs) {return holder}} defn cl-head (xs) {(h r) = (../xs @2) {return h}} defn cl-tail (xs) {(h r) = (../xs @2) {return r}} cl-nil = :nil xs0 = cl-nil xs1 = (cl-cons 42 xs0) xs2 = (cl-cons 34 xs1) a = (cl-head xs2) b = (cl-head (cl-tail xs2)) {return `(~a ~b)}} '(34 42)
fntest {defn fff (x) {return (/std/+ x 42)} yyy = (fff 1) {return yyy}} 43
fntest {defn fff () {x = 2 defn ggg () {return (/std/+ x 40)} {return ggg}} ggg-in-fff = (fff @mut) {return (fff/ggg-in-fff)}} 42
fntest {res = (/libc/print 42) {return 33}} 33
fntest {defn do-something (x) {res = (/libc/print x) {return 'do-something-value}} defn interceptor (arg) {(ext-pointer arg-) = (../do-something @mut @(host "call-extension-1") @2 arg) res = (/libc/print "extension:") res = (/libc/print ext-pointer) res = (/libc/print "argument:") res = (/libc/print arg-) host-res = {return @1 @(host "call-extension") {ext-pointer arg-}} () = (../interceptor @0 @something host-res)} res = (interceptor 'arg) {return res}} 'do-something-value
fntest {defn wrapper () {defn wrapped (x) {return `(~x ~x)} () = (wrapped @mut @pre @0 @up) {return 33}} () = (wrapper @mut @0) res = (wrapper 42) {return res}} '(42 42)
fntest {if 3 {return 42} {return 25}} 42
fntest {x = -5 while (/std/not (/std/eq x 0)) {x = (/std/+ x 1)} defn f (x) {return x} if 3 {return 42} {return 25}} 42
fntest {a = 5 {return a}} 5
defn print-all (xs)
{if xs
 {res = (/libc/print (/std/head xs))
  () = (../print-all @0 (/std/tail xs))
  return {}}
 {() = {return {}}}}

if panics
{() = (print-all @0 panics)
 () = (/std/panic @0 "FAILED")}

{}

x = "if at the end of the module doesn't work well, so here is this statement:)"
