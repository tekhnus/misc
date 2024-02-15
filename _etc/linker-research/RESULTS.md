on Linux:
- rpath pointint to out/ directory is added by wrapper by default
- linker doesn't follow symlinks
- relative -L paths will not propagate into the binary info

on Mac:
- linker follows symlinks
- relative -L paths will be encoded into the binary info
- dyld searches in the current directory first of all by default
