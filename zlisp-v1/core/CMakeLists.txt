add_library(
	zlisp-impl
	SHARED
	datum.c reading.c compiling.c running.c extending.c
	datum.h reading.h compiling.h running.h extending.h
	zlisp/common.h)

target_include_directories(zlisp-impl PUBLIC .)
target_include_directories(zlisp-impl PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
set_target_properties(zlisp-impl PROPERTIES
				 C_STANDARD 11
				 C_STANDARD_REQUIRED YES
				 C_EXTENSIONS YES)

add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/zlisp/common.h"
                  COMMAND bash -c "cp ${CMAKE_CURRENT_SOURCE_DIR}/*.c ."
									COMMAND rm -rf "${CMAKE_CURRENT_BINARY_DIR}/zlisp/common.h"
									COMMAND mkdir -p zlisp
                  COMMAND bash -c "$<TARGET_FILE:makeheaders> -H *.c >export.h"
                  COMMAND bash -c "echo '#pragma' once | cat - export.h >zlisp/common.h"
                  COMMAND rm export.h
									DEPENDS datum.c reading.c compiling.c running.c extending.c
                  VERBATIM)

add_custom_command(
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/datum.h"
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/reading.h"
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/compiling.h"
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/running.h"
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/extending.h"
									COMMAND rm -rf "${CMAKE_CURRENT_BINARY_DIR}/*.h"
                  COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/*.c .
                  COMMAND makeheaders -v *.c
									DEPENDS datum.c reading.c compiling.c running.c extending.c
                  )
