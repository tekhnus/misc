add_library(zlisp-build-lib SHARED main.c linking.c extensions.c building.c)
target_link_libraries(zlisp-build-lib PUBLIC zlisp-c)
target_compile_options(zlisp-build-lib PRIVATE -Wall -Wextra -Wno-unused-function -Werror)
target_include_directories(zlisp-build-lib PUBLIC .)

add_executable(zlisp-build main.c linking.c extensions.c building.c)
target_link_libraries(zlisp-build PUBLIC zlisp-c)
target_compile_options(zlisp-build PRIVATE -Wall -Wextra -Wno-unused-function -Werror)
target_include_directories(zlisp-build PUBLIC .)

add_executable(zlisp-fmt fmt.c)
target_link_libraries(zlisp-fmt PUBLIC zlisp-c)
target_compile_options(zlisp-fmt PRIVATE -Wall -Wextra -Wno-unused-function -Werror)
target_include_directories(zlisp-fmt PUBLIC .)

add_custom_target(codegen-builder
                  COMMAND makeheaders -v *.[ch]
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
