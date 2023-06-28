add_library(zlisp-impl SHARED datum.c reading.c compiling.c running.c extending.c)
target_include_directories(zlisp-impl PUBLIC .)
target_include_directories(zlisp-impl PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
set_target_properties(zlisp-impl PROPERTIES
				 C_STANDARD 11
				 C_STANDARD_REQUIRED YES
				 C_EXTENSIONS YES)

add_custom_target(codegen-zlisp-impl
                  COMMAND bash -c "cp ${CMAKE_CURRENT_SOURCE_DIR}/*.c ."
									COMMAND mkdir -p zlisp
                  COMMAND bash -c "$<TARGET_FILE:makeheaders> -H *.c >export.h"
                  COMMAND bash -c "echo '#pragma' once | cat - export.h >zlisp/common.h"
                  COMMAND rm export.h
                  VERBATIM)

add_custom_target(codegen-core
                  COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/*.c .
                  COMMAND makeheaders -v *.c
                  )
