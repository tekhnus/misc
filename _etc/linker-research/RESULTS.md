on Linux:
- rpath to out/ directory is added by nix wrapper by default
- linker doesn't follow symlinks
- relative -L paths will not propagate into the binary info
- ld doesn't search in the current directory

on Mac:
- apparantly no rpath is added by nix wrapper by default
- linker follows symlinks
- relative -L paths will be encoded into the binary info
- dyld searches in the current directory first of all by default and uses the relative -L path in this case
