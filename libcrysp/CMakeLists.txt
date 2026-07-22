cmake_minimum_required(VERSION 3.5)

project(libcrysp
        VERSION 1.0.0
        LANGUAGES Fortran C)

include("../common.cmake")

add_subdirectory(src)
