add_library(zlisp-impl SHARED main.c compiling.c running.c)
target_link_libraries(zlisp-impl dl)
target_include_directories(zlisp-impl PUBLIC .)
set_target_properties(zlisp-impl PROPERTIES
				 C_STANDARD 11
				 C_STANDARD_REQUIRED YES
				 C_EXTENSIONS YES)
target_compile_options(zlisp-impl PRIVATE -Wall -Wextra -Wno-unused-function -Werror)
add_custom_target(codegen-zlisp-impl
                  COMMAND cat main.c compiling.c running.c >zlisp-impl.c
                  COMMAND makeheaders -v zlisp-impl.c types.h
                  COMMAND mv zlisp-impl.h zlisp-impl/
                  COMMAND rm zlisp-impl.c
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
