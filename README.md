FortranUtilities is a collection of simple functions for Fortran programs.

Functions for strings, numbers, precision, statistics and files are included. See below for specifications of the different functions. More functions for different tasks will be included in the future.

This is a BETA version. All functions work properly on Linux using gfortran 7.5 or newer, and all functions except symlink functions work on Windows using gfortran 7.5 or newer. More compilers will be tested in the future.

## Downloading

   The latest version of this library can be obtained from the GitHub repository located [here](https://github.com/ecasglez/FortranUtilities).

## Compilation

1. Check that you have cmake version 3.10 or newer, a Fortran compiler compatible with Fortran 2008, and a C++ compiler compatible with C++17.
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
WRITE(*,*) int2str(5)
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

## Module Prec\_M

Contains precision parameters to use in Fortran programs.

* ```sp```: Kind parameter to specify a real type with a storage size of 32 bits.
* ```dp```: Kind parameter to specify a real type with a storage size of 64 bits.
* ```qp```: Kind parameter to specify a real type with a storage size of 128 bits.
* ```i8```: Kind parameter to specify an integer type with a storage size of 8 bits.
* ```i16```: Kind parameter to specify an integer type with a storage size of 16 bits.
* ```i32```: Kind parameter to specify an integer type with a storage size of 32 bits.
* ```i64```: Kind parameter to specify an integer type with a storage size of 64 bits.

## Module Numbers\_M

Contains useful tools to analyze numbers. These functions are now available in the intrinsics module IEEE_ARITHMETIC and are provided here only for compatibility with some old programs that use them.

* ```is_nan(num)```: Determines if the number is NaN or not.
   * ```num```: real number or array with real numbers to analye. It can have any rank and size.
   * ```returns```: True if the number is NaN. False otherwise. The return value will have the same rank and size as the input value.
* ```is_inf(num)```: Determines if the number is Infinite or not.
   * ```num```: real number or array with real numbers to analye. It can have any rank and size.
   * ```returns```: True if the number is Infinite. False otherwise. The return value will have the same rank and size as the input value.

## Module Strings\_M

Contains useful tools to manipulate strings in Fortran programs.

* ```num2str(num, formato)```: Converts an integer or real variable into a string. Useful to open files named sequentially.
   * ```num```: Number to convert.
   * ```formato```: only for real numbers, format to use in the string variable.
   * ```returns```: string containing the number.
* ```int2str00000(integ,total_length)```: Converts an integer variable into a string, filling with leading zeros up to the limit imposed by the user. Useful to open files named sequentially with leading zeros in the name.
   * ```integ```: Integer number to convert. This number MUST be positive.
   * ```total_length```: Number of digits to use including zeros.
   * ```returns```: string contining the number
* ```count_digits_integer(integ)```: Counts the number of digits of an integer, including the - sign in case it is a negative value.
   * ```integ```: Integer number whose digits are to be counted.
   * ```returns```: The number of digits of the input number.
* ```str2num(str,mold)```: Converts a string into an integer or a real.
   * ```str```: String to convert to integer or real
   * ```mold```: real or integer to specify the type of result. It is only used to set the type of the return value, so it can be any value.
   * ```returns```: Integer or real containing the number in the input string. The type is the same as the mold one.
* ```splitstr(str,fieldNumber,delimiter,rev)```: Splits a string and returns the portion selected by the user.
   * ```str```: String that the user wants to split.
   * ```fieldNumber```: Integer indicating which of the divisions to return.
   * ```delimiter```: String that the users wants to use as a delimiter for splitting. Optional parameter. Default is Space.
   * ```rev```: Logical variable. If True start spliting by the end of the string. Optional parameter. Default is False.
   * ```returns```: A string with the selected part of str. If the fieldNumber does not exists or if the delimiter does not exists it returns an empty string.
* ```startsWith(str,substr)```: Checks if a string starts with a given substring.
   * ```str```: String that the user wants to check how it starts.
   * ```substr```: Substring to search to check if str starts with it.
   * ```returns```: True if the string starts with the substring and False otherwise. If substr is empty it returns True.
* ```endsWith(str,substr)```: Checks if a string starts with a given substring.
   * ```str```: String that the user wants to check how it ends.
   * ```substr```: Substring to search to check if str ends with it.
   * ```returns```: True if the string ends with the substring and False otherwise. If substr is empty it returns True.

## Module Statistics\_M

Contains useful tools to perform simple statistics in Fortran programs. Input data is an array with one dimension. If your data is has more dimensions you can use RESHAPE or sequence association.

* ```mean(values)```: Calculates the mean value of a set of values given in an array.
   * ```values```: Vector of real numbers to calculate the mean value. It can have any size and it must have one dimension.
   * ```returns```: Real number with the average value.
* ```gmean(values)```: Calculates the geometric mean of a set of values given in an array.
   * ```values```: Vector of real numbers to calculate the geometric mean. It can have any size and it must have one dimension.
   * ```returns```: Real number with the geometric mean.
* ```variance(values)```: Calculates the sample variance of a set of values given in an array.
   * ```values```: Vector of real numbers to calculate the sample variance. It can have any size and it must have one dimension.
   * ```returns```: Real number with the sample variance.
* ```pvariance(values)```: Calculates the population variance of a set of values given in an array.
   * ```values```: Vector of real numbers to calculate the population variance. It can have any size and it must have one dimension.
   * ```returns```: Real number with the population variance.
* ```stdev(values)```: Calculates the sample standard deviation of a set of values given in an array.
   * ```values```: Vector of real numbers to calculate the sample standard deviation. It can have any size and it must have one dimension.
   * ```returns```: Real number with the sample standard deviation.
* ```pstdev(values)```: Calculates the population standard deviation of a set of values given in an array.
   * ```values```: Vector of real numbers to calculate the population standard deviation. It can have any size and it must have one dimension.
   * ```returns```: Real number with the population standard deviation.

## Module Files\_M

Contains useful tools to manage files.

* ```mkdir(dir,ignoreErrors)```. Creates a directory.
   * ```dir```: path and name of the directory to be created.
   * ```ignoreErrors```: True to print a detailed description of the error message. Optional variable. Default is False.
   * ```returns```: True if the process has been succesful. False in case of error.
* ```create_symlink(src,dest,ignoreErrors)```. Creates a symlink.
   * ```src```: path and name of the file or directory to be linked.
   * ```dest```: name of the destination link.
   * ```ignoreErrors```: True to print a detailed description of the error message. Optional variable. Default is False.
   * ```returns```: True if the process has been succesful. False in case of error.
* ```cp(src,dest,ignoreErrors)```. Copies a file or directory. Directories are copied recursively. Existing files are overwritten.
   * ```src```: path and name of the file to be copied.
   * ```dest```: destination.
   * ```ignoreErrors```: True to print a detailed description of the error message. Optional variable. Default is False.
   * ```returns```: True if the process has been succesful. False in case of error.
* ```rm(src,dest,ignoreErrors)```. Moves or renames a file or directory. When moving a directory if the destination has the same name and it is an already existing directory which isnot empty an error is shown and nothing is done. The destination folder must be removed first using rm.
   * ```src```: path and name of the file to be moved.
   * ```dest```: destination.
   * ```ignoreErrors```: True to print a detailed description of the error message. Optional variable. Default is False.
   * ```returns```: True if the process has been succesful. False in case of error.
* ```rm(fname,ignoreErrors)```. Removes a file or directory
   * ```fname```: path and name of the file to be removed.
   * ```ignoreErrors```: True to print a detailed description of the error message. Optional variable. Default is False.
   * ```returns```: True if the process has been succesful. False in case of error.
* ```exists(fname)```. Checks if a file or directory exists.
   * ```fname```: path and name of the file or directory to be checked.
   * ```returns```: True if the file or directory exists. False otherwise.
* ```is_directory(fname, ignoreErrors)```. Checks if a directory exists.
   * ```fname```: path of the directory to be checked.
   * ```ignoreErrors```: True to print a detailed description of the error message. Optional variable. Default is False.
   * ```returns```: True if the directory exists. False otherwise.
* ```is_empty(fname, ignoreErrors)```. Checks if a file or directory is empty.
   * ```fname```: path and name of the file or directory to be checked.
   * ```ignoreErrors```: True to print a detailed description of the error message. Optional variable. Default is False.
   * ```returns```: True if the file or directory is empty. False otherwise.
* ```is_regular_file(fname, ignoreErrors)```. Checks if a regular file (not a directory, symlink, etc.) exists.
   * ```fname```: path of the file to be checked.
   * ```ignoreErrors```: True to print a detailed description of the error message. Optional variable. Default is False.
   * ```returns```: True if the file exists. False otherwise.
* ```is_symlink(fname, ignoreErrors)```. Checks if a symlink exists.
   * ```fname```: path of the file to be checked.
   * ```ignoreErrors```: True to print a detailed description of the error message. Optional variable. Default is False.
   * ```returns```: True if the symlink exists. False otherwise.

## Contact

Developed by Emilio Castro.

Create a Issue in GitHub if you have any suggestion, comment, enhancement, bug, etc.

## License

See LICENSE file for more information on using and distributing these files.

