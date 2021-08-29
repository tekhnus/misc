add_executable(fek fek/main.cpp)
find_package(SDL2 REQUIRED)
target_link_libraries(fek PUBLIC SDL2::Main)
