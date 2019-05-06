cmake_minimum_required(VERSION 3.13)

project (nes_emulator)
set(CMAKE_BUILD_TYPE Release)

include_directories(nes_emulator)
add_library(nes_emulator SHARED src/nesemu1.cpp)