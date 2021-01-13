title: Calculate mean value of a rank 3 array.
author: Emilio Castro
date: 13/09/2020

## Description ##

This examples illustrate the use of ```[[FU_Prec]]``` and ```[[FU_Statistics]]``` modules using a simple program.
It generates an array with rank 3 filled with random numbers showing 3 examples on how to apply function ```[[mean]]```
(or any other function from [[FU_Statistics]]) to an array of size larger than 1.

## Functions used ##

* ```[[FU_Prec]]```.
    * ```dp```
* ```[[FU_Statistics]]```.
    * ```[[mean]]```

## Code ##

```Fortran
{!documentation/Examples/Example02/example2.f90!}
```

## Compilation ##

Compile using the following command. Adjust paths accordingly.

```Text
gfortran example2.f90 -o example2 -fopenmp -I/path/to/include/ -lFortranUtilities -L/path/to/lib/ -O2
```

## Execution ##

Before running, since it has been compiled against the shared library:

```Text
export LD_LIBRARY_PATH=/path/to/lib:${LD_LIBRARY_PATH}
```

Then run with:

```Text
./example2
```

The output of the execution is:

```Text
Mean value:   0.50046.
Mean value:   0.50046.
Mean value:   0.50046.
```
