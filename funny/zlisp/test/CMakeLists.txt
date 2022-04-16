add_test(NAME tests
         COMMAND zlisp-run ${CMAKE_CURRENT_SOURCE_DIR}/tests.lisp)
add_test(NAME print-file
         COMMAND zlisp-run ${CMAKE_CURRENT_SOURCE_DIR}/print-hosts.lisp)
add_test(NAME run-cli
         COMMAND sh -c "printf '(require \"std\")\n(+ 33 9)\n' | $<TARGET_FILE:zlisp-run> ${PROJECT_SOURCE_DIR}/module/cli/main.lisp")

set_tests_properties(
        tests print-file run-cli
        PROPERTIES
        ENVIRONMENT ZLISP=${PROJECT_SOURCE_DIR}/module)
