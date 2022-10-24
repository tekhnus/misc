add_test(NAME tests
         COMMAND bash -c "$<TARGET_FILE:zlisp-run> <($<TARGET_FILE:zlisp-build> c-prelude ${CMAKE_CURRENT_SOURCE_DIR}/tests.lisp)")
add_test(NAME print-file
         COMMAND bash -c "$<TARGET_FILE:zlisp-run> <($<TARGET_FILE:zlisp-build> c-prelude ${CMAKE_CURRENT_SOURCE_DIR}/print-hosts.lisp) | tee /dev/stderr | grep localhost")
add_test(NAME run-cli
         COMMAND bash -c "printf '(req (+ \"std\" +)) (+ 33 9)\n' | LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}/builder $<TARGET_FILE:zlisp-run> <($<TARGET_FILE:zlisp-build> c-prelude ${PROJECT_SOURCE_DIR}/module/cli/main.lisp) | tee /dev/stderr | grep 42")
add_test(NAME pytests
         COMMAND sh -c "$<TARGET_FILE:zlisp-build> py-prelude ${CMAKE_CURRENT_SOURCE_DIR}/pytest.lisp | ${PROJECT_SOURCE_DIR}/host-python/zlisp-run-py")

set_tests_properties(
        tests print-file run-cli pytests
        PROPERTIES
        ENVIRONMENT "ZLISP=${PROJECT_SOURCE_DIR}/module;PATH=$<TARGET_FILE_DIR:Python::Interpreter>:$ENV{PATH}")
