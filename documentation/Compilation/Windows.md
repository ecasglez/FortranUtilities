title: Compilation on Windows
author: Emilio Castro
date: 13/09/2020

### Requirements ###

* cmake 3.11 or newer.
* Only tested on Windows 10.
* One of the following options:
    * [MSYS2-Mingw-w64](#compilation-using-msys2-environment) environment with Gfortran and G++ compilers installed.
    * [equation.com](#compilation-using-equationcom-tools) compilers Gfortran, G++ and make and the Windows command prompt.
    * Other options and combinations could work but have not been tested.

### Compilation using MSYS2 environment ###

MSYS2 can be downloaded [here](https://www.msys2.org/). The following packages must be installed:

* mingw-w64-x86_64-ninja **OR** mingw-w64-x86_64-make.
* mingw-w64-x86_64-gcc-fortran **AND** mingw-w64-x86_64-gcc.
* mingw-w64-clang-x86_64-cmake.

The follow this steps to compile:

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

      By default, cmake will use ninja. If you want to use make type:

      ```
      cmake .. -G "Unix Makefiles"
      ```

3. Compile typing:

      ```
      ninja
      ```

      or

      ```
      make
      ```

4. Install:

      ```
      ninja install
      ```

      ```
      make install
      ```

### Compilation using equation.com tools ###

Binaries for Gfortran and G++ can be downloaded [here](http://equation.com).

1. Download the sources, open a Windows command prompt and enter directory FortranUtilities. Then create a build directory and enter that directory:

      ```
      mkdir build
      ```

      ```
      cd build
      ```

2. Prepare compilation files. If cmake, gfortran, gcc and g++ are in your PATH, to use default options on your system type:

      ```
      cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -G "Unix Makefiles"
      ```

      If any of the tools if not in your PATH, you must type the full path for it.

      Optionally you can select an install directory by typing:

      ```
      cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=C:\FortranUtilities
      ```

3. Compile typing:

      ```
      make
      ```

4. Install:

      ```
      make install
      ```
