cmake_minimum_required(VERSION 3.13)
project(nes_emulator)

add_executable(nes_emulator cpu.cpp cpu.h nesemu1.cpp nesemu1.h)