add_executable(zlisp-run main.c builtins.c compiling.c host.c)
target_link_libraries(zlisp-run PUBLIC zlisp-impl)
add_library(zlisp-c SHARED builtins.c compiling.c host.c)
target_link_libraries(zlisp-c PUBLIC zlisp-impl)
target_include_directories(zlisp-run PUBLIC .)
target_include_directories(zlisp-c PUBLIC .)
add_custom_target(codegen-zlisp-cli-bootstrap
                  COMMAND makeheaders -v *.[ch]
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
