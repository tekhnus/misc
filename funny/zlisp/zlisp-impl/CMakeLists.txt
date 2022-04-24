add_library(zlisp-impl SHARED main.c compiling.c running.c)
target_include_directories(zlisp-impl PUBLIC .)
set_target_properties(zlisp-impl PROPERTIES
				 C_STANDARD 11
				 C_STANDARD_REQUIRED YES
				 C_EXTENSIONS YES)
target_compile_options(zlisp-impl PRIVATE -Wall -Wextra -Wno-unused-function -Werror)
add_custom_target(codegen-zlisp-impl
                  COMMAND bash -c "cat main.c compiling.c running.c >zlisp-impl.c"
                  COMMAND makeheaders -v zlisp-impl.c types.h
                  COMMAND bash -c "echo '#pragma' once | cat - zlisp-impl.h >zlisp-impl/zlisp-impl.h"
                  COMMAND rm zlisp-impl.c
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                  VERBATIM)
