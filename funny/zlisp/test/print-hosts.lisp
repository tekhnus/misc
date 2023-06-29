req
{{prelude "prelude"}
 {fopen "libc" fopen}
 {libc "libc"}
 {stdout "libc" stdout}
 {malloc "libc" malloc}
 {fread "libc" fread}
 {fprintf-pointer "libc" fprintf-pointer}}

hostsfile := (/prelude/fopen "/etc/hosts" "r")

buffer := (/prelude/malloc 1024000)

xxx := (/prelude/fread buffer 1 1024 hostsfile)

yyy := (/prelude/fprintf-pointer stdout buffer)
