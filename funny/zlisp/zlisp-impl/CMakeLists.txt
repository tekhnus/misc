add_library(zlisp-impl SHARED zlisp-impl/main.c zlisp-impl/compiling.c zlisp-impl/main.h)
find_path(FFI_INCLUDE_DIRS ffi.h PATH_SUFFIXES ffi)
target_link_libraries(zlisp-impl ffi dl)
target_include_directories(zlisp-impl PUBLIC . ${FFI_INCLUDE_DIRS})
set_target_properties(zlisp-impl PROPERTIES
				 C_STANDARD 11
				 C_STANDARD_REQUIRED YES
				 C_EXTENSIONS YES)
target_compile_options(zlisp-impl PRIVATE -Wall -Wextra)
add_custom_target(codegen COMMAND makeheaders -v zlisp-impl/main.c zlisp-impl/compiling.c zlisp-impl/types.h WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
