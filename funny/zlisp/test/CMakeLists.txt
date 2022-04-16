add_test(NAME tests
         COMMAND zlisp-run ${CMAKE_CURRENT_SOURCE_DIR}/tests.lisp)
add_test(NAME print-file
         COMMAND zlisp-run ${CMAKE_CURRENT_SOURCE_DIR}/print-hosts.lisp)
add_test(NAME run-cli
         COMMAND sh -c "printf '(require \"std\")\n(+ 33 9)\n' | ${CMAKE_CURRENT_BINARY_DIR}/../zlisp-cli-bootstrap/zlisp-run ${CMAKE_CURRENT_SOURCE_DIR}/../module/cli/main.lisp")
