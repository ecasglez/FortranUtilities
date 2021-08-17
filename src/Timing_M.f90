!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------

MODULE FU_Timing
   !! author: Emilio Castro.
   !! date: 10/09/2020.
   !! version: 1.0.
   !! license: MIT.
   !! summary: Tools to measure time spent by functions in Fortran programs.
   !! Tools to measure time spent by functions in Fortran programs.
   !! The user can use IntervalTime to get the time since the last measure or the time
   !! since the begining of the execution if no previous measuement is available. This point
   !! will be used as a starting point for the next interval.
   !! Use TotalTime to get the time since the beginning of the program or since the last time
   !! resetTotalTime is executed.

   USE FU_Prec

   IMPLICIT NONE

   PRIVATE 
   PUBLIC :: ResetTotalTime, TotalTime, IntervalTime

   INTERFACE
      SUBROUTINE c_ResetTotalTime() BIND(c,name='c_ResetTotalTime')
         USE iso_c_binding
      END SUBROUTINE c_ResetTotalTime
      FUNCTION c_TotalTime_sp() RESULT(res) BIND(c,name='c_TotalTime_sp')
         USE iso_c_binding
         REAL(KIND=C_FLOAT) :: res
      END FUNCTION c_TotalTime_sp
      FUNCTION c_TotalTime_dp() RESULT(res) BIND(c,name='c_TotalTime_dp')
         USE iso_c_binding
         REAL(KIND=C_DOUBLE) :: res
      END FUNCTION c_TotalTime_dp
      FUNCTION c_IntervalTime_sp() RESULT(res) BIND(c,name='c_IntervalTime_sp')
         USE iso_c_binding
         REAL(KIND=C_FLOAT) :: res
      END FUNCTION c_IntervalTime_sp
      FUNCTION c_IntervalTime_dp() RESULT(res) BIND(c,name='c_IntervalTime_dp')
         USE iso_c_binding
         REAL(KIND=C_DOUBLE) :: res
      END FUNCTION c_IntervalTime_dp
   END INTERFACE

   INTERFACE TotalTime
      !! author: Emilio Castro.
      !! date: 10/09/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Gets the time in seconds since the beginning of the program or since the last time resetTotalTime is executed.
      !! TotalTime gets the time in seconds (with a precision of microseconds)
      !! since the beginning of the program or since the last time [[ResetTotalTime]] is executed.
      !!
      !! You can use mold input argument to indicate the precision of the output number. Default precision
      !! is the default precision of your compiler.
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! t = TotalTime(mold)
      !!```
      !!
      !! Where:
      !!
      !! * `mold`: Real value to identify the kind of the output. Optional.
      !! It is only used to set the kind of the return value, so it can be any value.
      !! 
      !! Total time spent in seconds since the begining of the program or since the last
      !! time [[ResetTotalTime]] has been used. Uses precision set by mold.
      !!
      !!### Example
      !!
      !! The following program prints the total time since the start of the program.
      !! A more detailed example can be found at [here](../page/Examples/Example01/index.html).
      !!
      !!```Fortran
      !! PROGRAM totalTimeExample
      !!    USE FU_Timing, ONLY: TotalTime
      !!    USE FU_Prec, ONLY:dp
      !!    IMPLICIT NONE
      !!    WRITE(*,*) TotalTime()
      !!    WRITE(*,*) TotalTime(1._dp)
      !! END PROGRAM totalTimeExample
      !!```
      MODULE PROCEDURE TotalTime_def
      MODULE PROCEDURE TotalTime_sp
      MODULE PROCEDURE TotalTime_dp
   END INTERFACE TotalTime

   INTERFACE IntervalTime
      !! author: Emilio Castro.
      !! date: 10/09/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Gets the time in seconds since the last measurement.
      !! IntervalTime gets the time in seconds (with a precision of microseconds) 
      !! since the last measurement or the time since the begining of the execution if no previous 
      !! measuement is available. This point will be used as a starting point for the next interval.
      !! The starting point can be reset using [[ResetTotalTime]].
      !!
      !! You can use mold input argument to indicate the precision of the output number. Default precision
      !! is the default precision of your compiler.
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! t = IntervalTime(mold)
      !!```
      !!
      !! Where:
      !!
      !! * `mold`: Real value to identify the kind of the output. Optional.
      !! It is only used to set the kind of the return value, so it can be any value.
      !! 
      !! Time spent in seconds since the last measurement, or since the begining of the program 
      !! if no previous measurement is available, or since the last time
      !! [[ResetTotalTime]] has been used. Uses precision set by mold. 
      !!
      !!### Example
      !!
      !! The following program prints the time since the start of the program and since
      !! the previous measurement.
      !! A more detailed example can be found at [here](../page/Examples/Example01/index.html).
      !!
      !!```Fortran
      !! PROGRAM intervalTimeExample
      !!    USE FU_Timing, ONLY: IntervalTime
      !!    USE FU_Prec, ONLY:dp
      !!    IMPLICIT NONE
      !!    WRITE(*,*) IntervalTime()
      !!    WRITE(*,*) IntervalTime(1._dp)
      !! END PROGRAM intervalTimeExample
      !!```
      MODULE PROCEDURE IntervalTime_def
      MODULE PROCEDURE IntervalTime_sp
      MODULE PROCEDURE IntervalTime_dp
   END INTERFACE IntervalTime

   CONTAINS


      SUBROUTINE ResetTotalTime()
         !! author: Emilio Castro.
         !! date: 10/09/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Sets the starting point to count the total time.
         !! Sets the starting point to count the total time. [[IntervalTime]] and
         !! [[TotalTime]] are reset.
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! CALL ResetTotalTime()
         !!```
         !!### Example
         !!
         !! The following program prints the total time since the start of the program, resets
         !! the total time counter and then measures it again.
         !! A more detailed example can be found at [here](../page/Examples/Example01/index.html).
         !!
         !!```Fortran
         !! PROGRAM resetTotalTimeExample
         !!    USE FU_Timing, ONLY: TotalTime, ResetTotalTime
         !!    USE FU_Prec, ONLY:dp
         !!    IMPLICIT NONE
         !!    WRITE(*,*) TotalTime()
         !!    CALL ResetTotalTime()
         !!    WRITE(*,*) TotalTime()
         !! END PROGRAM resetTotalTimeExample
         !!```
         IMPLICIT NONE
         CALL c_ResetTotalTime()
      END SUBROUTINE ResetTotalTime


      FUNCTION TotalTime_def() RESULT(res)
         IMPLICIT NONE
         REAL :: res
         !! Total time spent in seconds since the begining of the program or since the last
         !! time ResetTotalTime has been used. Uses default precision of the compiler used.
         res = REAL(c_TotalTime_dp()) / 1.E6
      END FUNCTION TotalTime_def

      FUNCTION TotalTime_sp(mold) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp), INTENT(IN) :: mold
         !! Real value to identify the kind of the output.
         !! It is only used to set the kind of the return value, so it can be any value.
         REAL(KIND=sp) :: res
         !! Total time spent in seconds since the begining of the program or since the last
         !! time ResetTotalTime has been used. Uses precision set by mold.
         IF (.FALSE.) res = mold !To disable compilation warning about unused variable
         res = c_TotalTime_sp() / 1.E6_sp
      END FUNCTION TotalTime_sp

      FUNCTION TotalTime_dp(mold) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp), INTENT(IN) :: mold
         !! Real value to identify the kind of the output.
         !! It is only used to set the kind of the return value, so it can be any value.
         REAL(KIND=dp) :: res
         !! Total time spent in seconds since the begining of the program or since the last
         !! time ResetTotalTime has been used. Uses precision set by mold.
         IF (.FALSE.) res = mold !To disable compilation warning about unused variable
         res = c_TotalTime_dp() / 1.E6_dp
      END FUNCTION TotalTime_dp


      FUNCTION IntervalTime_def() RESULT(res)
         IMPLICIT NONE
         REAL :: res
         !! Time spent in seconds since the last measurement or since the begining of the program 
         !! if no previous measurement is available. Uses default precision of the compiler used.
         res = REAL(c_IntervalTime_dp()) / 1.E6
      END FUNCTION IntervalTime_def

      FUNCTION IntervalTime_sp(mold) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=sp), INTENT(IN) :: mold
         !! Real value to identify the kind of the output.
         !! It is only used to set the kind of the return value, so it can be any value.
         REAL(KIND=sp) :: res
         !! Time spent in seconds since the last measurement or since the begining of the program 
         !! if no previous measurement is available. Uses precision set by mold. 
         IF (.FALSE.) res = mold !To disable compilation warning about unused variable
         res = c_IntervalTime_sp() / 1.E6_sp
      END FUNCTION IntervalTime_sp

      FUNCTION IntervalTime_dp(mold) RESULT(res)
         IMPLICIT NONE
         REAL(KIND=dp), INTENT(IN) :: mold
         !! Real value to identify the kind of the output.
         !! It is only used to set the kind of the return value, so it can be any value.
         REAL(KIND=dp) :: res
         !! Time spent in seconds since the last measurement or since the begining of the program 
         !! if no previous measurement is available. Uses precision set by mold. 
         IF (.FALSE.) res = mold !To disable compilation warning about unused variable
         res = c_IntervalTime_dp() / 1.E6_dp
      END FUNCTION IntervalTime_dp

END MODULE FU_Timing
