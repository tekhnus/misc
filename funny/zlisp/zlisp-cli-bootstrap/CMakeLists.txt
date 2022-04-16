add_executable(zlisp-run main.c builtins.c compiling.c host.c)
target_link_libraries(zlisp-run PUBLIC zlisp-impl ffi dl)
add_library(zlisp-c SHARED builtins.c compiling.c host.c)
target_link_libraries(zlisp-c PUBLIC zlisp-impl ffi dl)
find_path(FFI_INCLUDE_DIRS ffi.h PATH_SUFFIXES ffi)
target_include_directories(zlisp-run PUBLIC . ${FFI_INCLUDE_DIRS})
target_include_directories(zlisp-c PUBLIC . ${FFI_INCLUDE_DIRS})
add_custom_target(codegen-zlisp-cli-bootstrap
                  COMMAND makeheaders -v *.[ch]
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
