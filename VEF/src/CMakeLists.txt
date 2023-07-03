cmake_minimum_required(VERSION 3.0)

project(alamDMC Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

if   (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(alamDMC_FLAGS "-ffree-line-length-0")
else()
    message(FATAL_ERROR "Compiler type(CMAKE_Fortran_COMPILER_ID) not recognized")
endif()

list(APPEND Fortran_FLAGS ${alamDMC_FLAGS})

add_executable(alamDMC
nlls_tr.f90
alamYLP.f90 
alamEval.f90
alamYLPConstants.f90
abstractModule.f90
alamEvalCached.f90
basicModule.f90
commonConfig.f90
commonUtils.f90
deformationDrivenModule.f90
dmcResultFileOutput.f90
evolutionOutputRecord.f90
incrementationControl.f90
qrsTypes.f90
resultTable.f90
resultTableRecord.f90
strainDrivenStep.f90
stressDrivenEvolutionModule.f90
stressDrivenModule.f90
substeppingConfig.f90
xVectorEvolutionOutputRecord.f90
YLPResult.f90
dmcADP.f90
dmcASR.f90 
dmcEWC.f90 
dmcQRS.f90  
dmcUDSA.f90  
dmcYld.f90  
alamDMC.f90
)

target_link_libraries(alamDMC altay)
set_target_properties(alamDMC PROPERTIES LINK_FLAGS "-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl")

install(TARGETS alamDMC RUNTIME DESTINATION bin)
