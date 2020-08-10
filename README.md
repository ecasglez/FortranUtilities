FortranUtilities is a collection of simple functions for Fortran programs.

Functions for strings, numbers, precision, statistics and files are included. See the specifications of the different functions. More functions for different tasks will be included in the future.

This is a BETA version. All functions work properly on Linux using gfortran 7.5 or newer, and all functions except symlink functions work on Windows using gfortran 7.5 (MSYS2-Mingw-w64) or newer. More compilers will be tested in the future.

## Downloading

   The latest version of this library can be obtained from the GitHub repository located [here](https://github.com/ecasglez/FortranUtilities).

## Compilation

1. Check that you have cmake version 3.10 or newer, a Fortran compiler compatible with Fortran 2008, and a C++ compiler compatible with C++17.
2. Download the files and enter directory FortranUtilities. Then create a build directory and enter that directory:

      ```
      mkdir build
      ```

      ```
      cd build
      ```

3. Type:

      ```
      cmake ..
      ```
   
      Optionally you can select an install directory by typing:
   
      ```
      cmake .. -DCMAKE_INSTALL_PREFIX=/installation/path/
      ```

      On Windows using MSYS2-Mingw-w64 use:

      ```
      cmake .. -G "MinGW Makefiles"
      ```


4. Compile:

      ```
      make
      ```

      On Windows using MSYS2-Mingw-w64 use instead of make:

      ```
      mingw32-make.exe
      ```

5. Install:

      ```
      make install
      ```

A static and and a shared library are created. Test programs for both libraries are created too.

## Documentation

Documentation of the different functions is [here](https://ecasglez.github.io/FortranUtilities/).

## Usage

To use the library in your programs you first need to use the module of interest, as in the following example:

```fortran
PROGRAM test
USE FU_Strings
WRITE(*,*) int2str(5)
END PROGRAM test
```

Then you have to link to the library when compiling. For example:

1. To use the static library:

      ```
      g++ program.f90 -o program -lgfortran -lFortranUtilitiesStatic -L/path/to/the/library/folder -I/path/to/include/folder -lstdc++fs
      ```

2. To use the shared library:

      ```
      gfortran program.f90 -lFortranUtilities -L/path/to/the/library/folder -I/path/to/include/folder
      ```

## Contact

Developed by Emilio Castro.

Create a Issue in GitHub if you have any suggestion, comment, enhancement, bug, etc.

## License

These files are distributed under a MIT license. See LICENSE file for more information on using and distributing these files.

