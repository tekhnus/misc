add_executable(fek fek/main.cpp)
set_property(TARGET fek PROPERTY CXX_STANDARD 20)
find_package(SDL2 REQUIRED)
target_link_libraries(fek PUBLIC SDL2::Main)
