cmake_minimum_required(VERSION 3.10)

project (FortranUtilities VERSION 1.0 DESCRIPTION "Useful functions for Fortran programs")

enable_language (Fortran)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "7.5.0")
	message(FATAL_ERROR "GNU fortran 7.5.0 or newer required")
  endif()
endif()

add_compile_options(-Wall -O2)

add_subdirectory(src)
