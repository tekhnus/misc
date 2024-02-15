on Linux:
- rpath to out/ directory is added by nix wrapper by default
- linker doesn't follow symlinks
- the binary info contains only library basename without path,
  regardless of how -L is specified
- ld doesn't search in the current directory
- linker flag -rpath changes DT_RPATH; it automatically affects all libraries
- relative DT_RPATH will be relative to the current directory
- magic '$ORIGIN/...' DT_RPATH will be relative to the directory
  of the executable

on Mac:
- apparantly no rpath is added by nix wrapper by default
- linker follows symlinks
- the binary info contains the relative/absolute path to library,
  depending of how -L is specified
- linker flag -rpath changes @rpath; it doesn't automatically affect anything;
- one should use install_name_tool to make particular libraries
  to get prefixes with @rpath, @loader_path or @executable_path
- dyld first searches the path as it is given;
  if it's relative, it's interpreted relative to the current directory;
  if it starts with @rpath, @loader_path or @executable_path,
  it'll be treated accordingly.
