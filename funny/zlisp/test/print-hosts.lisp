req
{{prelude "prelude"}
 {fopen "libc" fopen}
 {libc "libc"}
 {stdout-val "libc" stdout-val}
 {malloc "libc" malloc}
 {fread "libc" fread}
 {fprintf-pointer-new "libc" fprintf-pointer-new}}

hostsfile := (/prelude/fopen "/etc/hosts" "r")

buffer := (/prelude/malloc 1024000)

xxx := (/prelude/fread buffer 1 1024 hostsfile)

yyy := (/prelude/fprintf-pointer-new stdout-val buffer)
