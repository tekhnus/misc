cmake_minimum_required(VERSION 3.17)

project(fek VERSION 0.1 LANGUAGES CXX)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/sdl2)
add_compile_options(-Wall -Wextra -pedantic)

add_subdirectory(fek)
