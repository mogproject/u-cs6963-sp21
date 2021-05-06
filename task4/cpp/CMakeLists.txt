# OPTIL.io uses CMake version 3.5.1, as of April 11, 2021.
# see https://www.optil.io/optilion/environment
cmake_minimum_required(VERSION 3.5)

# options
option(BUILD_TESTS "Build test programs" OFF)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# project
project(Exact)
add_executable(Exact src/exact.cpp)

# tests with GoogleTest
if (BUILD_TESTS)
  add_subdirectory(test)
endif (BUILD_TESTS)
