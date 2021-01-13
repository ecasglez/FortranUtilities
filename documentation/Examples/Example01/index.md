title: Timing and Statistics usage (with OpenMP)
author: Emilio Castro
date: 13/09/2020

## Description ##

This example illustrates the use of ```[[FU_Prec]]```, ```[[FU_Statistics]]``` and ```[[FU_Timing]]``` modules using a simple program.
It generates a set of random numbers and then performs a lot of mean, variance and median calculations, and measures the time needed with different number of threads (using OpenMP). 

Uses up to 7 OpenMP threads. Change this parameter accordingly.

## Functions used ##

* ```[[FU_Prec]]```.
    * ```dp```
* ```[[FU_Statistics]]```.
    * ```[[mean]]```
    * ```[[variance]]```
    * ```[[median]]```
* ```[[FU_Timing]]```.
    * ```[[resetTotalTime]]```
    * ```[[IntervalTime]]```
    * ```[[TotalTime]]```

## Code ##

```Fortran
{!documentation/Examples/Example01/example1.f90!}
```

## Compilation ##

Compile using the following command. Adjust paths accordingly.

```Text
gfortran example1.f90 -o example1 -fopenmp -I/path/to/include/ -lFortranUtilities -L/path/to/lib/ -O2
```

If no OpenMP libraries are available in your system, you can remove option ```-fopenmp```. The code will work but there will not be any speedup.

## Execution ##

Before running, since it has been compiled against the shared library:

```Text
export LD_LIBRARY_PATH=/path/to/lib:${LD_LIBRARY_PATH}
```

Then run with:

```Text
./example1
```

The output of the execution is:

```Text
Number of threads: 1. Time spent:  22.010 s.
Number of threads: 2. Time spent:  11.440 s.
Number of threads: 3. Time spent:   8.107 s.
Number of threads: 4. Time spent:   6.113 s.
Number of threads: 5. Time spent:   5.887 s.
Number of threads: 6. Time spent:   4.987 s.
Number of threads: 7. Time spent:   4.452 s.
Total time spent:  62.995 s.
```
