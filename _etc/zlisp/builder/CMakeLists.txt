add_library(zlisp-build-lib SHARED
  linking.c extensions.c building.c
  linking.h extensions.h building.h
)
target_link_libraries(zlisp-build-lib PUBLIC zlisp-c)
target_include_directories(zlisp-build-lib PUBLIC .)
target_include_directories(zlisp-build-lib PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

add_executable(zlisp-build builder_main.c builder_main.h)
target_link_libraries(zlisp-build PUBLIC zlisp-c zlisp-build-lib)
target_include_directories(zlisp-build PUBLIC .)
target_include_directories(zlisp-build PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

add_executable(zlisp-fmt fmt.c fmt.h)
target_link_libraries(zlisp-fmt PUBLIC zlisp-c zlisp-build-lib)
target_include_directories(zlisp-fmt PUBLIC .)
target_include_directories(zlisp-fmt PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

add_custom_command(
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/builder_main.h"
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/linking.h"
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/extensions.h"
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/building.h"
									OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/fmt.h"
									COMMAND rm -rf "${CMAKE_CURRENT_BINARY_DIR}/*.h"
									COMMAND rm -rf "${CMAKE_CURRENT_BINARY_DIR}/zlisp/*.h"
                  COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/*.[ch] .
                  COMMAND makeheaders -v *.[ch]
                  DEPENDS
   linking.c extensions.c building.c builder_main.c fmt.c
)