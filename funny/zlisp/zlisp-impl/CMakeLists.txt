add_library(zlisp-impl SHARED main.c compiling.c running.c)
target_include_directories(zlisp-impl PUBLIC .)
set_target_properties(zlisp-impl PROPERTIES
				 C_STANDARD 11
				 C_STANDARD_REQUIRED YES
				 C_EXTENSIONS YES)
target_compile_options(zlisp-impl PRIVATE -Wall -Wextra -Wno-unused-function -Werror)
add_custom_target(codegen-zlisp-impl
                  COMMAND bash -c "cat main.c compiling.c running.c >extern.c"
                  COMMAND makeheaders -v extern.c types.h
                  COMMAND rm extern.c
                  COMMAND bash -c "$<TARGET_FILE:makeheaders> -H main.c compiling.c running.c types.h >export.h"
                  COMMAND bash -c "echo '#pragma' once | cat - export.h >zlisp-impl/zlisp-impl.h"
                  COMMAND rm export.h
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                  VERBATIM)
