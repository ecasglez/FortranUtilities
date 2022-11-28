!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------

MODULE FU_Interpolation
   !! author: Emilio Castro.
   !! date: 24/11/2022.
   !! version: 1.0.
   !! license: MIT.
   !! summary: Functions to perform interpolation.
   !! Functions to perform interpolation.

   USE FU_Prec

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: lin_interp

   INTERFACE lin_interp
      !! author: Emilio Castro.
      !! date: 24/11/2022.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Performs linear interpolation.
      !! Performs linear interpolation between the two nearest points in the dataset.
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! y = lin_interp(x, known_xs, known_ys)
      !!```
      !!
      !! Where:
      !!
      !! * `x`: Data point to be predicted.
      !! * `known_xs`: Independent array of data, known X values.
      !! * `known_ys`: Dependent array of data, known Y values.
      !! 
      !! It returns the predicted value y at position x, by linear interpolation
      !! between the nearest points to x in known_xs.
      !!
      !! If `known_xs` and `known_ys` do not have the same size, it will return nan.
      !! If `known_xs` is not ascending ordered, it will return nan.
      !!
      !! Note that this function does not extrapolate.
      !! * If x is lower than known_xs(1), the returned value will be known_ys(1).
      !! * If x is larger than known_xs(N), the returned value will be known_ys(N), where
      !! N is the last index in the arrays.
      !!
      !!### Example
      !!
      !! The following program performs a linear interpolation:
      !!
      !!```Fortran
      !! PROGRAM lin_interpExample
      !!    USE FU_Interpolation, ONLY: lin_interp
      !!    IMPLICIT NONE
      !!    REAL,DIMENSION(6) :: X = (/1., 2., 3., 4., 5., 6./)
      !!    REAL,DIMENSION(6) :: Y = (/0.9, 2.6, 4.3, 8.2, 10.0, 11.1/)
      !!    WRITE(*,*) lin_interp(4.4., X, Y)
      !! END PROGRAM lin_interpExample
      !!```
      MODULE PROCEDURE lin_interp_sp
      MODULE PROCEDURE lin_interp_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE lin_interp_qp
#endif
   END INTERFACE lin_interp

   CONTAINS

   PURE FUNCTION lin_interp_sp(x, known_xs, known_ys) RESULT(y)
      USE FU_statistics, ONLY: linreg
      USE FU_Arrays, ONLY: is_ordered
      IMPLICIT NONE
      INTEGER, PARAMETER :: prec = sp
      !! Data point to be predicted
      REAL(KIND=prec), INTENT(IN) :: x
      !! Independent array of data, known X values.
      REAL(KIND=prec), DIMENSION(:), INTENT(IN) :: known_xs
      !! Dependent array of data, known Y values.
      REAL(KIND=prec), DIMENSION(:), INTENT(IN) :: known_ys
      !! Predictad value at position x
      REAL(KIND=prec) :: y
      INTEGER :: i_1, i_2, siz
      REAL(KIND=prec) :: a, b, R2
      REAL(KIND=prec) :: zero

      
      INCLUDE 'Interpolation_M/include_lin_interp.f90'
      
   END FUNCTION lin_interp_sp

   PURE FUNCTION lin_interp_dp(x, known_xs, known_ys) RESULT(y)
      USE FU_statistics, ONLY: linreg
      USE FU_Arrays, ONLY: is_ordered
      IMPLICIT NONE
      INTEGER, PARAMETER :: prec = dp
      !! Data point to be predicted
      REAL(KIND=prec), INTENT(IN) :: x
      !! Independent array of data, known X values.
      REAL(KIND=prec), DIMENSION(:), INTENT(IN) :: known_xs
      !! Dependent array of data, known Y values.
      REAL(KIND=prec), DIMENSION(:), INTENT(IN) :: known_ys
      !! Predictad value at position x
      REAL(KIND=prec) :: y
      INTEGER :: i_1, i_2, siz
      REAL(KIND=prec) :: a, b, R2
      REAL(KIND=prec) :: zero

      
      INCLUDE 'Interpolation_M/include_lin_interp.f90'
      
   END FUNCTION lin_interp_dp

#ifdef QPREC_FPP
   PURE FUNCTION lin_interp_qp(x, known_xs, known_ys) RESULT(y)
      USE FU_statistics, ONLY: linreg
      USE FU_Arrays, ONLY: is_ordered
      IMPLICIT NONE
      INTEGER, PARAMETER :: prec = qp
      !! Data point to be predicted
      REAL(KIND=prec), INTENT(IN) :: x
      !! Independent array of data, known X values.
      REAL(KIND=prec), DIMENSION(:), INTENT(IN) :: known_xs
      !! Dependent array of data, known Y values.
      REAL(KIND=prec), DIMENSION(:), INTENT(IN) :: known_ys
      !! Predictad value at position x
      REAL(KIND=prec) :: y
      INTEGER :: i_1, i_2, siz
      REAL(KIND=prec) :: a, b, R2
      REAL(KIND=prec) :: zero

      
      INCLUDE 'Interpolation_M/include_lin_interp.f90'
      
   END FUNCTION lin_interp_qp
#endif

END MODULE FU_Interpolation
