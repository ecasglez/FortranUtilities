title: Compilation
author: Emilio Castro
date: 13/09/2020

Ecasglez's FortranUitilities has been tested in both Windows and Linux platforms using different compilers.

## Linux ##

#### Requirements ####

* cmake 3.11 or newer.
* A Fortran compiler compatible with Fortran 2008. Tested with:
    * Gfortran 7.5 or newer.
    * Intel Fortran compiler 16 or newer.
    * Flang compiler from AMD AOCC.
    * PGI compiler 20 or newer from Nvidia HPC-SDK.
* A C++ compiler compatible with C++ 17. Tested with:
    * G++ 7.5 or newer.
    * Intel C++ compiler 19 or newer.
    * Clang 6 or newer, oficial clang or clang from AMD AOCC.
    * PGI compiler 20 or newer from Nvidia HPC-SDK.

#### Compilation ####

1. Download the sources and enter directory FortranUtilities. Then create a build directory and enter that directory:

      ```
      mkdir build
      ```

      ```
      cd build
      ```

2. Prepare compilation files. To use default options and compiler on your system type:

      ```
      cmake ..
      ```

      Optionally you can select an install directory by typing:

      ```
      cmake .. -DCMAKE_INSTALL_PREFIX=/installation/path/
      ```

      You can specify a different compiler by typing:

      ```
      CC=icc CXX=icc FC=ifort cmake ..
      ```

      You can use a Fortran compiler and a C++ compiler from a different vendor. For example, to use Intel Fortran compiler and G++, type:

      ```
      CC=gcc CXX=g++ FC=ifort cmake ..
      ```

3. Compile typing:

      ```
      make
      ```

4. Install:

      ```
      make install
      ```

A static and a shared library are created. Tests programs for both libraries are created too.



## Windows ##

#### Requirements ####

* cmake 3.11 or newer.
* MSYS2-Mingw-w64 environment with Gfortran and G++ compilers installed.

#### Compilation ####

1. Download the sources, open MSYS2-Mingw-w64 terminal and enter directory FortranUtilities. Then create a build directory and enter that directory:

      ```
      mkdir build
      ```

      ```
      cd build
      ```

2. Prepare compilation files. To use default options and compiler on your system type:

      ```
      cmake ..
      ```

      Optionally you can select an install directory by typing:

      ```
      cmake .. -DCMAKE_INSTALL_PREFIX=/installation/path/
      ```

3. Compile typing:

      ```
      make
      ```

4. Install:

      ```
      make install
      ```
