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

