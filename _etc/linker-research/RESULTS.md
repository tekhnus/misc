on Linux:
- rpath to out/ directory is added by nix wrapper by default
- linker doesn't follow symlinks
- regardless of how -L is specified, the binary info contains
  only library name, not the path to it
- ld doesn't search in the current directory

on Mac:
- apparantly no rpath is added by nix wrapper by default
- linker follows symlinks
- relative -L paths will be encoded into the binary info
- dyld searches in the current directory first of all by default and uses the relative -L path in this case
