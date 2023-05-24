req
{{prelude "prelude"}
 {fopen "libc" fopen}
 {malloc "libc" malloc}
 {fread "libc" fread}
 {printfptr "libc" printfptr}}

hostsfile = {call {/prelude/fopen "/etc/hosts" "r"}}
buffer = {call {/prelude/malloc 2048}}
xxx = {call {/prelude/fread buffer 1 1024 hostsfile}}
yyy = {call {/prelude/printfptr "%.2048s" buffer}}
