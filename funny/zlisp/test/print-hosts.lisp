(req
 (prelude "prelude")
 (fopen "libc" fopen)
 (malloc "libc" malloc)
 (fread "libc" fread)
 (printfptr "libc" printfptr)
 (ignore "std" ignore))

(def hostsfile (prelude/fopen "/etc/hosts" "r"))
(def buffer (prelude/malloc 2048))
(ignore (prelude/fread buffer 1 1024 hostsfile))
(ignore (prelude/printfptr "%.2048s" buffer))
