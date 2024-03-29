cmake_minimum_required(VERSION 3.12)

project(zlisp VERSION 0.1 LANGUAGES C)
enable_testing()

find_package(Python COMPONENTS Interpreter)

# Without this gdb doesn't skip function prologues
# https://stackoverflow.com/questions/64697087/gdb-shows-incorrect-arguments-of-functions-for-stack-frames
add_compile_options(-fcf-protection=none)
add_compile_options(-Wall -Wextra -Werror)
add_compile_options(-fsanitize=address)
add_link_options(-fsanitize=address)
add_compile_options(-fsanitize=undefined -fno-sanitize-recover=undefined)
add_link_options(-fsanitize=undefined -fno-sanitize-recover=undefined)

add_subdirectory(core)
add_subdirectory(host-ffi)
add_subdirectory(builder)
add_subdirectory(host-python)
add_subdirectory(tools)
add_subdirectory(test)
