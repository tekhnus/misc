add_library(zlisp-impl SHARED zlisp-impl/main.c zlisp-impl/main.h zlisp-impl/prelude.lisp)
target_include_directories(zlisp-impl PUBLIC .)
set_target_properties(zlisp-impl PROPERTIES
				 C_STANDARD 11
				 C_STANDARD_REQUIRED YES
				 C_EXTENSIONS NO)
