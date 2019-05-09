CXX=g++
CXXFLAGS=-Wall -W -pedantic -Wno-int-in-bool-context  -O3 -std=c++17 `pkg-config sdl --libs --cflags`

nes_simulator: cpu_simple_example.cpp
	$(CXX) -o "$@" "$<" $(CXXFLAGS)
