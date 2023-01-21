(req
 (prelude "prelude")
 (fopen "libc" fopen)
 (malloc "libc" malloc)
 (fread "libc" fread)
 (printfptr "libc" printfptr))

(def hostsfile (prelude/fopen "/etc/hosts" "r"))
(def buffer (prelude/malloc 2048))
(def xxx (prelude/fread buffer 1 1024 hostsfile))
(def yyy (prelude/printfptr "%.2048s" buffer))
