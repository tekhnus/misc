cmake_minimum_required(VERSION 3.12)

project(zlisp VERSION 0.1 LANGUAGES C)
enable_testing()

find_package(Python COMPONENTS Interpreter)

add_subdirectory(zlisp-impl)
add_subdirectory(host-ffi)
add_subdirectory(host-python)
add_subdirectory(tools)
add_subdirectory(test)

add_custom_target(codegen
                  DEPENDS codegen-zlisp-impl codegen-host-ffi codegen-host-python)
