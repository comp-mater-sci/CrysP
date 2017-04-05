cmake_minimum_required(VERSION 3.7)
project(slisadmin VERSION 1.0 LANGUAGES CXX)

add_executable(slisadmin slisadmin.cpp  stdafx.cpp)
target_compile_definitions(slisadmin PRIVATE LICENSE_KULEUVEN_INTERNAL)
target_link_libraries(slisadmin PUBLIC slis)

install(TARGETS slisadmin DESTINATION "bin")
