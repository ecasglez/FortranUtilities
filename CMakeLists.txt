cmake_minimum_required(VERSION 3.10)

project (FortranUtilities VERSION 1.0 DESCRIPTION "Useful functions for Fortran programs")

enable_language (Fortran)
set (CMAKE_CXX_STANDARD 17)
set (CMAKE_CXX_STANDARD_REQUIRED ON)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "7.5.0")
	message(FATAL_ERROR "GNU fortran 7.5.0 or newer required")
  endif()
endif()
if (CMAKE_COMPILER_IS_GNUCXX)
   execute_process(COMMAND ${CMAKE_CXX_COMPILER} -dumpversion OUTPUT_VARIABLE GCC_VERSION)
   if (GCC_VERSION VERSION_GREATER 8.0 OR GCC_VERSION VERSION_EQUAL 8.0)
      add_definitions(-DGCC8)
   else ()
      add_definitions(-DGCC7)
   endif()
endif()

if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
   message("-- Detecting platform: Windows")
   add_definitions(-DWIN_CPP)
else ()
   message("-- Detecting platform: Unix-based")
   add_definitions(-DLIN_CPP)
endif ()

add_compile_options(-Wall -O2 -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans)

add_subdirectory(src)
