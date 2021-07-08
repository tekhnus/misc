add_library(zlisp-impl-wrapper SHARED zlisp-impl-wrapper/main.c)
target_link_libraries(zlisp-impl-wrapper PUBLIC zlisp-impl)
