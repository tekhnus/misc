add_library(zlisp-c SHARED
  builtins_.c running_.c
  builtins_.h running_.h
)
target_link_libraries(zlisp-c PUBLIC zlisp-impl ffi dl)

add_executable(zlisp-run main.c main.h)
target_link_libraries(zlisp-run PUBLIC zlisp-c)

find_path(FFI_INCLUDE_DIRS ffi.h PATH_SUFFIXES ffi)
target_include_directories(zlisp-c PUBLIC . ${FFI_INCLUDE_DIRS})
target_include_directories(zlisp-c PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

add_custom_command(
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/builtins_.h"
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/running_.h"
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/main.h"
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/zlisp/host-ffi.h"
                  COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/*.[ch] .
                  COMMAND makeheaders -v *.[ch]
                  COMMAND mkdir -p zlisp
                  COMMAND makeheaders -h -v *.[ch] >zlisp/host-ffi.h
                  )


