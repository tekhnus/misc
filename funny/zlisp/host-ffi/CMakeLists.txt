add_library(zlisp-c SHARED builtins.c running.c)
target_link_libraries(zlisp-c PUBLIC zlisp-impl ffi dl)

add_executable(zlisp-run main.c)
target_link_libraries(zlisp-run PUBLIC zlisp-c)

find_path(FFI_INCLUDE_DIRS ffi.h PATH_SUFFIXES ffi)
target_include_directories(zlisp-c PUBLIC . ${FFI_INCLUDE_DIRS})

add_custom_target(codegen-host-ffi
                  COMMAND makeheaders -v *.[ch]
                  COMMAND makeheaders -h -v *.[ch] >zlisp/host-ffi.h
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})


