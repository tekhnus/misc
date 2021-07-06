cmake_minimum_required(VERSION 3.7)

project(zlisp VERSION 0.1 LANGUAGES C)

add_subdirectory(zlisp-impl)
add_subdirectory(zlisp-cli-bootstrap)
add_subdirectory(module)
