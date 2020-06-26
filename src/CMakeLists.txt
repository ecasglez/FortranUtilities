
set(LibrarySources Strings_M.f90 Prec_M.f90 Statistics_M.f90 Numbers_M.f90 Files_M.F90 cfiles.cpp)
set(TestSources FortranUtilitiesTest.F90)

set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod/)

#Library
add_library(objlib OBJECT ${LibrarySources})
add_library (FortranUtilitiesStatic STATIC $<TARGET_OBJECTS:objlib>)
target_link_libraries(FortranUtilitiesStatic stdc++fs)
add_library (FortranUtilities SHARED $<TARGET_OBJECTS:objlib>)
target_link_libraries(FortranUtilities stdc++fs)
set_property(TARGET objlib PROPERTY POSITION_INDEPENDENT_CODE 1)

#Tests
add_library(objtests OBJECT ${TestSources})
add_dependencies(objtests FortranUtilities)
add_executable(FortranUtilitiesTestStatic $<TARGET_OBJECTS:objtests>)
target_link_libraries(FortranUtilitiesTestStatic FortranUtilitiesStatic)
add_executable(FortranUtilitiesTest $<TARGET_OBJECTS:objtests>)
target_link_libraries(FortranUtilitiesTest FortranUtilities)

#
set_target_properties(objlib objtests
	PROPERTIES Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR})


install(TARGETS FortranUtilities FortranUtilitiesStatic 
	FortranUtilitiesTestStatic FortranUtilitiesTest
	RUNTIME DESTINATION bin
	ARCHIVE DESTINATION lib
	LIBRARY DESTINATION lib
	)

install(DIRECTORY ${LIB_MOD_DIR} DESTINATION include)
