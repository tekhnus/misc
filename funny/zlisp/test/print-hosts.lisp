req
{{prelude "prelude"}
 {dereference "prelude" dereference}
 {fopen "libc" fopen}
 {libc "libc"}
 {malloc "libc" malloc}
 {fread "libc" fread}
 {print "libc" print}}

hostsfile := (/prelude/fopen "/etc/hosts" "r")

buffer := (/prelude/malloc 2048)

xxx := (/prelude/fread buffer 1 1024 hostsfile)

bufstr := (/prelude/dereference buffer 'string)

yyy := (/libc/print bufstr)
