add_library(zlisp-build-lib SHARED builder_main.c linking.c extensions.c building.c)
target_link_libraries(zlisp-build-lib PUBLIC zlisp-c)
target_include_directories(zlisp-build-lib PUBLIC .)
target_include_directories(zlisp-build-lib PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

add_executable(zlisp-build builder_main.c linking.c extensions.c building.c)
target_link_libraries(zlisp-build PUBLIC zlisp-c)
target_include_directories(zlisp-build PUBLIC .)
target_include_directories(zlisp-build PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

add_executable(zlisp-fmt fmt.c extensions.c building.c linking.c)
target_link_libraries(zlisp-fmt PUBLIC zlisp-c)
target_include_directories(zlisp-fmt PUBLIC .)
target_include_directories(zlisp-fmt PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

add_custom_target(codegen-builder
                  COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/*.[ch] .
                  COMMAND makeheaders -v *.[ch])