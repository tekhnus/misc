add_custom_command(OUTPUT zlisp-impl/prelude.c
	        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
		COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/zlisp-impl
		COMMAND xxd -i zlisp-impl/prelude.lisp ${CMAKE_CURRENT_BINARY_DIR}/zlisp-impl/prelude.c
		DEPENDS zlisp-impl/prelude.lisp)
add_library(zlisp-impl SHARED zlisp-impl/main.c zlisp-impl/main.h zlisp-impl/prelude.c)
find_path(FFI_INCLUDE_DIRS ffi.h PATH_SUFFIXES ffi)
target_link_libraries(zlisp-impl ffi dl)
target_include_directories(zlisp-impl PUBLIC . ${FFI_INCLUDE_DIRS})
set_target_properties(zlisp-impl PROPERTIES
				 C_STANDARD 11
				 C_STANDARD_REQUIRED YES
				 C_EXTENSIONS YES)
