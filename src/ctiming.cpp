/* Fortran Utilities
 *
 * MODULE: Timing_M
 *
 * DESCRIPTION:
 * Functions to measure time intervals.
 *
 * REVISION HISTORY:
 * 10-09-2020 - Initial Version.
 *
 * AUTHOR: Emilio Castro.
 *
 * VERSION 1.0.
 *
 * Copyright: See LICENSE file that comes with this distribution.
 *
*/

#include <chrono>

std::chrono::steady_clock::time_point InitialTime = std::chrono::steady_clock::now();
std::chrono::steady_clock::time_point PreviousTime = std::chrono::steady_clock::now();

extern "C" 
{
   void c_ResetTotalTime();
}

void c_ResetTotalTime()
{
   InitialTime = std::chrono::steady_clock::now();
   PreviousTime = InitialTime;
}

extern "C" 
{
   float c_TotalTime_sp();
}

float c_TotalTime_sp()
{
   std::chrono::steady_clock::time_point NewTime = std::chrono::steady_clock::now();
   return std::chrono::duration_cast<std::chrono::microseconds>(NewTime - InitialTime).count();
}

extern "C" 
{
   float c_IntervalTime_sp();
}

float c_IntervalTime_sp()
{
   std::chrono::steady_clock::time_point NewTime = std::chrono::steady_clock::now();
   std::chrono::steady_clock::time_point PreviousTime2 = PreviousTime;
   PreviousTime = NewTime;
   return std::chrono::duration_cast<std::chrono::microseconds>(NewTime - PreviousTime2).count();
}

extern "C" 
{
   double c_TotalTime_dp();
}

double c_TotalTime_dp()
{
   std::chrono::steady_clock::time_point NewTime = std::chrono::steady_clock::now();
   return std::chrono::duration_cast<std::chrono::microseconds>(NewTime - InitialTime).count();
}

extern "C" 
{
   double c_IntervalTime_dp();
}

double c_IntervalTime_dp()
{
   std::chrono::steady_clock::time_point NewTime = std::chrono::steady_clock::now();
   std::chrono::steady_clock::time_point PreviousTime2 = PreviousTime;
   PreviousTime = NewTime;
   return std::chrono::duration_cast<std::chrono::microseconds>(NewTime - PreviousTime2).count();
}

