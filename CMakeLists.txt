cmake_minimum_required(VERSION 3.13)

project (nes_emulator)

set(prefix "/usr/local/Cellar/sdl/1.2.15_1")
set(exec_prefix "${prefix}")
set(libdir "${exec_prefix}/lib")
set(SDL_PREFIX "/usr/local/Cellar/sdl/1.2.15_1")
set(SDL_EXEC_PREFIX "/usr/local/Cellar/sdl/1.2.15_1")
set(SDL_LIBDIR "${exec_prefix}/lib")
set(SDL_INCLUDE_DIRS "${prefix}/include/SDL")
set(SDL_LIBRARIES "-L${SDL2_LIBDIR}  -lSDL")
string(STRIP "${SDL_LIBRARIES}" SDL_LIBRARIES)

set(CMAKE_BUILD_TYPE Debug)

#include_directories(nes_emulator)
include_directories(/usr/local/Cellar/sdl/1.2.15_1/include/SDL)
include_directories(/usr/local/Cellar/sdl_image/1.2.12_7/include/SDL)

#target_link_libraries(nes_emulator ${SDL_LIBRARIES})
#link_directories(/usr/local/Cellar/sdl2/2.0.9_1/lib)
#add_executable(nes_emulator src/cpu_simple_example.cpp)
add_executable(nes_emulator src/nes_emulator.cpp)
