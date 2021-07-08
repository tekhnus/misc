add_executable(zlisp-run zlisp-cli-bootstrap/main.c)
target_link_libraries(zlisp-run PUBLIC zlisp-impl zlisp-impl-wrapper ffi dl)
