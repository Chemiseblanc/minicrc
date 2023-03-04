cmake_minimum_required(VERSION 3.20)

if(DEFINED PROJECT_NAME)
    set(IS_SUBPROJECT true)
endif()

project(minicrc
    VERSION 1.0.0
    DESCRIPTION "Header-only library for computing cyclic redundancy check codes"
    LANGUAGES CXX)

include(FetchContent)

FetchContent_Declare(
    AddLib
    GIT_REPOSITORY https://github.com/Chemiseblanc/AddLib.git
    GIT_TAG v2.1.2
)
FetchContent_MakeAvailable(AddLib)

include(AddLib)
include(CTest)

add_lib(minicrc
    HEADER_ONLY
    SOURCES
        include/minicrc/minicrc.hpp
    INCLUDE_DIRS
        ${CMAKE_CURRENT_SOURCE_DIR}/include
    COMPILE_FEATURES
        cxx_std_20
    TESTS
        test/main.cpp
)

install_project()
package_project(
    CONTACT "Matthew Gibson <matt@mgibson.ca>"
    LICENSE_FILE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt"
)