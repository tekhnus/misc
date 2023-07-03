req
{{prelude "prelude"}
 {ser "prelude" ser}
 {fopen "libc" fopen}
 {libc "libc"}
 {stdout-val "libc" stdout-val}
 {malloc "libc" malloc}
 {fread "libc" fread}
 {fprintf-pointer-new "libc" fprintf-pointer-new}}

hostsfile := (/libc/fopen "/etc/hosts" "r")

sz := (/prelude/ser 1024000)

one := (/prelude/ser 1)

filelen := (/prelude/ser 1024)

warning := "TODO: convert those from int64_t to size_t"

buffer := (/prelude/malloc sz)

xxx := (/prelude/fread buffer one filelen hostsfile)

yyy := (/prelude/fprintf-pointer-new stdout-val buffer)
