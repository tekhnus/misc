(req
 (prelude "prelude")
 (fopen "libc" fopen)
 (malloc "libc" malloc)
 (fread "libc" fread)
 (printfptr "libc" printfptr))

(hostsfile = (/prelude/fopen "/etc/hosts" "r"))
(buffer = (/prelude/malloc 2048))
(xxx = (/prelude/fread buffer 1 1024 hostsfile))
(yyy = (/prelude/printfptr "%.2048s" buffer))
