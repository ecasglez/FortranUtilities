cmake_minimum_required(VERSION 3.10)

project (FortranUtilities VERSION 1.0 DESCRIPTION "Useful functions for Fortran programs")

enable_language (Fortran)
set (CMAKE_CXX_STANDARD 17)
set (CMAKE_CXX_STANDARD_REQUIRED ON)

#
#Check build type
#
set(default_build_type "Release")
if(NOT CMAKE_BUILD_TYPE)
   set(CMAKE_BUILD_TYPE "${default_build_type}")
endif()
set(ACCEPTED_BUILD_TYPES Release Debug RelWithDebInfo)
list(FIND ACCEPTED_BUILD_TYPES ${CMAKE_BUILD_TYPE} IS_BUILD_TYPE_ACCEPTED)
if(${IS_BUILD_TYPE_ACCEPTED} EQUAL -1)
  message(FATAL_ERROR "CMAKE_BUILD_TYPE of ${CMAKE_BUILD_TYPE} is not accepted.")
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

#
#Check for GNU compilers
#
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "7.5.0")
	message(FATAL_ERROR "GNU fortran 7.5.0 or newer required")
  endif()
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")
  set (CMAKE_Fortran_FLAGS_DEBUG "-g")
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS "7.5.0")
	message(FATAL_ERROR "GNU G++ 7.5.0 or newer required")
  endif()
  if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "8.0.")
     add_definitions(-DGCC7)
  else ()
     add_definitions(-DGCC8)
  endif()
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans")
  set (CMAKE_CXX_FLAGS_RELEASE "-O2")
  set (CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")
  set (CMAKE_CXX_FLAGS_DEBUG "-g")
endif()

#
#Check for Intel compilers
#
#Tested with Intel Fortran Compiler 16 and 19. Could work with older versions
if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "16")
      message(FATAL_ERROR "Intel Fortran Compiler 16 or newer required")
  endif()
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -warn all")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")
  set (CMAKE_Fortran_FLAGS_DEBUG "-g")
endif()

#Tested with Intel C++ Compiler 19. Could work with older versions
if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
  if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS "19")
     message(FATAL_ERROR "Intel C++ Compiler 19 or newer required")
  endif()
  add_definitions(-DICC)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -diag-disable:remark -pedantic")
  set (CMAKE_CXX_FLAGS_RELEASE "-O2")
  set (CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")
  set (CMAKE_CXX_FLAGS_DEBUG "-g")
endif()

#
#Check for Clang compilers
#
# Tested with Clang 6 and 10
if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS "6")
     message(FATAL_ERROR "Clang++ 6 or newer required")
  endif()
  add_definitions(-DCLANG)
  #  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans")
  set (CMAKE_CXX_FLAGS_RELEASE "-O2")
  set (CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")
  set (CMAKE_CXX_FLAGS_DEBUG "-g")
endif()

#
#Check for compilation platform
#
if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
   message(STATUS "Detecting platform: Windows")
   add_definitions(-DWIN_CPP)
else ()
   message(STATUS "Detecting platform: Unix-based")
   add_definitions(-DLIN_CPP)
endif ()

add_subdirectory(src)

