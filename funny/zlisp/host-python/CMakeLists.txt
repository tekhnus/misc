add_executable(zlisp-python-asm main.c)
target_link_libraries(zlisp-python-asm PUBLIC zlisp-c)
target_compile_options(zlisp-python-asm PRIVATE -Wall -Wextra -Wno-unused-function -Werror)
target_include_directories(zlisp-python-asm PUBLIC .)

add_custom_target(codegen-host-python
                  COMMAND makeheaders -v *.[ch]
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
