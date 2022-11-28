
set(LibrarySources Strings_M.F90 Prec_M.F90 Statistics_M.F90 Numbers_M.F90 Files_M.F90
    Timing_M.f90 Arrays_M.F90 Interpolation_M.F90 cfiles.cpp csort.cpp ctiming.cpp)
set(TestSources FortranUtilitiesTest.F90)

set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod)

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

set_property(TARGET FortranUtilitiesTest PROPERTY LINKER_LANGUAGE Fortran)
set_property(TARGET FortranUtilitiesTestStatic PROPERTY LINKER_LANGUAGE Fortran)

#On Windows, install all files in the same folder,
#On Linux, install files using GNU folder structure.
if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
   install(TARGETS FortranUtilities FortranUtilitiesStatic 
	FortranUtilitiesTestStatic FortranUtilitiesTest
	RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}
	ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}
	LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}
	)
   install(DIRECTORY ${LIB_MOD_DIR} DESTINATION ${CMAKE_INSTALL_PREFIX})
else ()
   install(TARGETS FortranUtilities FortranUtilitiesStatic 
	FortranUtilitiesTestStatic FortranUtilitiesTest
	RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
	ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
	LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
	)
   install(DIRECTORY ${LIB_MOD_DIR} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
endif ()
