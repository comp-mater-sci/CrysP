cmake_minimum_required(VERSION 3.3)

project(altay-binaries Fortran)

message(STATUS "altay-binaries: CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 
list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG) 
list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

add_executable(cub2cur 
	cub2cur.f90 
	cubAccess.f90
)

add_executable(cub2cubV2 
	cub2cubV2.f90 
	cubAccess.f90
)

add_executable(cub2smt 
	cub2smt.f90
	smtAccess.f90 
	cubAccess.f90
)

add_executable(ori2smt 
	ori2smt.f90
	oriAccess.f90
	smtAccess.f90 
	cubAccess.f90
)

add_executable(euler2cur 
	euler2cur.f90 
	eulerAccess.f90
)

add_executable(smt2cur 
	smt2cur.f90
	smtAccess.f90 
	cubAccess.f90
)

add_executable(formatCub 
	formatCub.f90 
	cubAccess.f90
)

add_executable(smt2cub 
	smt2cub.f90
	smtAccess.f90 	
	cubAccess.f90
)
 
add_executable(mergeCubs 
	mergeCubs.f90 
	cubAccess.f90
)
target_include_directories(mergeCubs PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(mergeCubs PRIVATE altay CRI -mkl)

#
# Installation
#
install(TARGETS cub2cur cub2cubV2 cub2smt ori2smt euler2cur smt2cur formatCub smt2cub mergeCubs
	DESTINATION bin
)

