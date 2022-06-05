(req
 (fopen "libc" fopen)
 (malloc "libc" malloc)
 (fread "libc" fread)
 (printfptr "libc" printfptr)
 (ignore "std" ignore))

!(req (stdmacro "stdmacro"))

(def hostsfile (fopen "/etc/hosts" "r"))
(def buffer (malloc 2048))
(ignore (fread buffer 1 1024 hostsfile))
(ignore (printfptr "%.2048s" buffer))

(export)
