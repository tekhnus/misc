add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/fonts/Menlo.ttc
	        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
		COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/fonts
		COMMAND cp fonts/Menlo.ttc ${CMAKE_CURRENT_BINARY_DIR}/fonts/Menlo.ttc
		DEPENDS fonts/Menlo.ttc)
add_executable(fek fek/main.cpp ${CMAKE_CURRENT_BINARY_DIR}/fonts/Menlo.ttc)
set_property(TARGET fek PROPERTY CXX_STANDARD 20)
if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Emscripten")
target_link_options(fek PUBLIC -sUSE_SDL=2 -sUSE_SDL_TTF=2)
target_link_options(fek PUBLIC -sNO_DISABLE_EXCEPTION_CATCHING)
target_link_options(fek PUBLIC --preload-file=fonts/Menlo.ttc)
set_target_properties(fek PROPERTIES SUFFIX ".html")
else()
find_package(SDL2 REQUIRED)
find_package(SDL2_ttf REQUIRED)
target_link_libraries(fek PUBLIC SDL2::Main SDL2::TTF)
endif()
