add_executable(zlisp-run zlisp-cli-bootstrap/main.c)
target_link_libraries(zlisp-run PUBLIC zlisp-impl zlisp-impl-wrapper)
add_test(NAME tests
         COMMAND zlisp-run ${CMAKE_CURRENT_SOURCE_DIR}/test/tests.lisp)
add_test(NAME print-file
         COMMAND zlisp-run ${CMAKE_CURRENT_SOURCE_DIR}/test/print-hosts.lisp)
