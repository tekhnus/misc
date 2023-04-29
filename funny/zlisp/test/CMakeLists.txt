add_test(NAME tests
         COMMAND bash -c "\
         BYTECODE=$(mktemp); \
         $<TARGET_FILE:zlisp-build> c-prelude ${CMAKE_CURRENT_SOURCE_DIR}/tests.lisp >$BYTECODE || exit 1; \
         $<TARGET_FILE:zlisp-run> $BYTECODE; \
         ")
add_test(NAME print-file
         COMMAND bash -c "\
         BYTECODE=$(mktemp); \
         $<TARGET_FILE:zlisp-build> c-prelude ${CMAKE_CURRENT_SOURCE_DIR}/print-hosts.lisp >$BYTECODE || exit 1; \
         $<TARGET_FILE:zlisp-run> $BYTECODE | tee /dev/stderr | grep localhost; \
         ")
add_test(NAME run-cli
         COMMAND bash -c "\
         BYTECODE=$(mktemp); \
         $<TARGET_FILE:zlisp-build> c-prelude ${PROJECT_SOURCE_DIR}/module/cli/main.lisp >$BYTECODE || exit 1; \
         printf '(req (std \"std\") (+ \"std\" +)) (return @halt @0 (/std/+ 33 9))\n' | LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}/builder $<TARGET_FILE:zlisp-run> $BYTECODE | tee /dev/stderr | grep 42; \
         ")
add_test(NAME pytests
         COMMAND bash -c "\
         BYTECODE=$(mktemp); \
         $<TARGET_FILE:zlisp-build> py-prelude ${CMAKE_CURRENT_SOURCE_DIR}/pytest.lisp >$BYTECODE || exit 1; \
         cat $BYTECODE | ${PROJECT_SOURCE_DIR}/host-python/zlisp-run-py; \
         ")

set_tests_properties(
        tests print-file run-cli pytests
        PROPERTIES
        ENVIRONMENT "ZLISP=${PROJECT_SOURCE_DIR}/module;PATH=$<TARGET_FILE_DIR:Python::Interpreter>:$ENV{PATH}")
