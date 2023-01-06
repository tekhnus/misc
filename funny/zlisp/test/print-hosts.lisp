(req
 (prelude "prelude")
 (fopen "libc" fopen)
 (malloc "libc" malloc)
 (fread "libc" fread)
 (printfptr "libc" printfptr)
 (ignore "std" ignore))

(def hostsfile (prelude @slash fopen "/etc/hosts" "r"))
(def buffer (prelude @slash malloc 2048))
(ignore (prelude @slash fread buffer 1 1024 hostsfile))
(ignore (prelude @slash printfptr "%.2048s" buffer))
