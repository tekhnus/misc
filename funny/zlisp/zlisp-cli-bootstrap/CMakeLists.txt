add_executable(zlisp-run zlisp-cli-bootstrap/main.c zlisp-cli-bootstrap/builtins.c zlisp-cli-bootstrap/compiling.c zlisp-cli-bootstrap/host.c)
target_link_libraries(zlisp-run PUBLIC zlisp-impl)
add_library(zlisp-c SHARED zlisp-cli-bootstrap/builtins.c zlisp-cli-bootstrap/compiling.c zlisp-cli-bootstrap/host.c)
target_link_libraries(zlisp-c PUBLIC zlisp-impl)
target_include_directories(zlisp-run PUBLIC .)
target_include_directories(zlisp-c PUBLIC .)
add_custom_target(codegen-zlisp-cli-bootstrap
                  COMMAND makeheaders -v zlisp-cli-bootstrap/*.[ch]
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
