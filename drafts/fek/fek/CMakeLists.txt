add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/fonts/Menlo.ttc
	        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
		COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/fonts
		COMMAND cp fonts/Menlo.ttc ${CMAKE_CURRENT_BINARY_DIR}/fonts/Menlo.ttc
		DEPENDS fonts/Menlo.ttc)
add_executable(fek fek/main.cpp ${CMAKE_CURRENT_BINARY_DIR}/fonts/Menlo.ttc)
set_property(TARGET fek PROPERTY CXX_STANDARD 20)
# find_package(SDL2 REQUIRED)
# find_package(SDL2_ttf REQUIRED)
target_link_libraries(fek PUBLIC SDL2 SDL2_ttf)
set_target_properties(fek PROPERTIES SUFFIX ".html")
