!(require "stdmacro")
(require "libc")

!(#def-or-panica printfptr
   (extern-pointer libc "printf"
		   '((string pointer) sizet)))
(def hostsfile (fopen "/etc/hosts" "r"))
(def buffer (malloc 2048))
(ignore (fread buffer 1 1024 hostsfile))
(ignore (printfptr "%.2048s" buffer))
