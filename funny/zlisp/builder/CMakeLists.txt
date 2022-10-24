add_executable(zlisp-build main.c)
target_link_libraries(zlisp-build PUBLIC zlisp-c)
target_compile_options(zlisp-build PRIVATE -Wall -Wextra -Wno-unused-function -Werror)
target_include_directories(zlisp-build PUBLIC .)

add_custom_target(codegen-builder
                  COMMAND makeheaders -v *.[ch]
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
