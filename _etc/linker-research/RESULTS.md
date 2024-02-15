on Linux:
- rpath to out/ directory is added by nix wrapper by default
- linker doesn't follow symlinks
- the binary info contains only library basename without path,
  regardless of how -L is specified
- ld doesn't search in the current directory

on Mac:
- apparantly no rpath is added by nix wrapper by default
- linker follows symlinks
- the binary info contains the relative/absolute path to library,
  depending of how -L is specified
- dyld first searches the path as it is given;
  if it's relative, it's interpreted relative to the current directory
