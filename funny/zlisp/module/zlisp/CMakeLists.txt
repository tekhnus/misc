add_library(module-zlisp-wrapper SHARED wrapper.c)
target_link_libraries(module-zlisp-wrapper PUBLIC zlisp-impl)
