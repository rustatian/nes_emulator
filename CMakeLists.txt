cmake_minimum_required(VERSION 3.13)

project (nes_emulator)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")


include(FindPkgConfig)
#(SDL Required)
#PKG_SEARCH_MODULE(SDL2 REQUIRED sdl2)
#PKG_SEARCH_MODULE(SDL2IMAGE REQUIRED SDL2_image>=2.0.0)

include_directories(${SDL_INCLUDE_DIRS} ${SDLIMAGE_INCLUDE_DIRS})
target_link_libraries(src/cpu_simple_example.cpp ${SDL_LIBRARIES} ${SDLIMAGE_LIBRARIES})

set(CMAKE_BUILD_TYPE Debug)

add_executable(nes_emulator src/cpu_simple_example.cpp)
