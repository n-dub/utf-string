cmake_minimum_required(VERSION 3.19)
project(utf-string)

set(CMAKE_CXX_STANDARD 17)

add_library(utf-string INTERFACE)
target_include_directories(utf-string INTERFACE
    include
)

if (UTF_STRING_BUILD_SAMPLES)
	add_subdirectory(Samples)
endif ()
