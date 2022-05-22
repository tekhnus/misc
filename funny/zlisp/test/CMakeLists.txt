add_test(NAME tests
         COMMAND zlisp-run ${CMAKE_CURRENT_SOURCE_DIR}/tests.lisp)
add_test(NAME print-file
         COMMAND zlisp-run ${CMAKE_CURRENT_SOURCE_DIR}/print-hosts.lisp)
add_test(NAME run-cli
         COMMAND sh -c "printf '(+ 33 9)\n' | $<TARGET_FILE:zlisp-run> ${PROJECT_SOURCE_DIR}/module/cli/main.lisp")
add_test(NAME pytests
         COMMAND sh -c "$<TARGET_FILE:zlisp-python-asm> ${CMAKE_CURRENT_SOURCE_DIR}/pytest.lisp | ${PROJECT_SOURCE_DIR}/host-python/asm_to_python")

set_tests_properties(
        tests print-file run-cli pytests
        PROPERTIES
        ENVIRONMENT "ZLISP=${PROJECT_SOURCE_DIR}/module;PATH=$<TARGET_FILE_DIR:Python::Interpreter>:$ENV{PATH}")
