cmake_minimum_required(VERSION 3.13)

project (nes_emulator)
set(CMAKE_BUILD_TYPE Release)

include_directories(nes_emulator)
include_directories(/usr/local/Cellar/sdl2/2.0.9_1/include/SDL2)
add_library(nes_emulator SHARED src/nesemu1.cpp)
