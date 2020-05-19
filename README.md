FortranUtilities is a collection of simple functions and subroutines for Fortran programs.

Only functions to manipulate strings are included so far. See file refman.pdf in folder doc for specifications of the different functions. More functions for different tasks will be included in the future.

## Downloading

   The latest version of this library can be obtained from the GitHub repository located [here](https://github.com/ecasglez/FortranUtilities).

## Compilation

1. Check that you have cmake version 3.10 or newer, and a Fortran compiler compatible with Fortran 2008.
2. Download the files and enter directory FortranUtilities. Then create a build directory and enter that directory:
   ```
   mkdir build
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

4. Compile:
   ```
   make
   ```

5. Install:
   ```
   make install
   ```

A static and and a shared library are created. Test programs for both libraries are created too.

## Usage

To use the library in your programs you first need to use the module of interest, as in the following example:

```fortran
PROGRAM test
USE Strings_M
WRITE(*,*) int2char(5)
END PROGRAM test
```

Then you have to link to the library when compiling. For example:

1. To use the static library:
   ```
   gfortran program.f90 -lFortranUtilitiesStatic -L/path/to/the/library/folder -I/path/to/include/folder
   ```

2. To use the shared library:
   ```
   gfortran program.f90 -lFortranUtilities -L/path/to/the/library/folder -I/path/to/include/folder
   ```

## Contact

Developed by Emilio Castro.

Create a Issue in GitHub if you have any suggestion, comment, enhancement, bug, etc.

## License

See LICENSE file for more information on using and distributing these files.
