/* Fortran Utilities
 *
 * MODULE: Statistics_M
 *
 * DESCRIPTION:
 * Sort function to be used in the median calculation of module Statistics_M.
 *
 * REVISION HISTORY:
 * 12-08-2020 - Initial Version.
 *
 * AUTHOR: Emilio Castro.
 *
 * VERSION 1.0.
 *
 * Copyright: See LICENSE file that comes with this distribution.
 *
*/

#include <algorithm>

extern "C"
{
   void c_sort_float(float *, int);
}

void c_sort_float(float * x, int n)
{
   std::sort(x, x + n);
}

extern "C"
{
   void c_sort_double(double *, int);
}

void c_sort_double(double * x, int n)
{
   std::sort(x, x + n);
}

extern "C"
{
   void c_sort_long_double(long double *, int);
}

void c_sort_long_double(long double * x, int n)
{
   std::sort(x, x + n);
}

