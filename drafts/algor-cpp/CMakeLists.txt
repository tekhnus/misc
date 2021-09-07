cmake_minimum_required(VERSION 3.17)

project(algor-cpp VERSION 0.1 LANGUAGES CXX)
add_compile_options(-Wall -Wextra -pedantic)

add_executable(test test/test_algor.cpp)

set_property(TARGET test PROPERTY CXX_STANDARD 20)
target_include_directories(test PUBLIC .)
