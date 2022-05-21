(req)

!(require "stdmacro")
(require "libc")

(def printfptr (c-function-or-panic libc "printf" '((string pointer) sizet)))

(def hostsfile (fopen "/etc/hosts" "r"))
(def buffer (malloc 2048))
(ignore (fread buffer 1 1024 hostsfile))
(ignore (printfptr "%.2048s" buffer))
